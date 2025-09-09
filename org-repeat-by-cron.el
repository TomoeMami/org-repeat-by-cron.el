;;; org-repeat-by-cron.el -*- lexical-binding: t; -*-
;;
;; Modified from org-reschedule-by-rule, DIFF:
;; 1. Use a simple elisp cron-parser instead of python package 'croniter'
;; 2. Replace 'INTERVAL' with 'DAY-AND' option

(require 'org)
(require 'subr-x)
(require 'cl-lib)

(defconst org-repeat-by-cron--month-aliases
  '(("jan" . "1") ("feb" . "2") ("mar" . "3") ("apr" . "4") ("may" . "5")
    ("jun" . "6") ("jul" . "7") ("aug" . "8") ("sep" . "9") ("oct" . "10")
    ("nov" . "11") ("dec" . "12"))
  "Mapping of month name abbreviations to numbers.")

(defconst org-repeat-by-cron--dow-aliases
  '(("sun" . "0") ("mon" . "1") ("tue" . "2") ("wed" . "3") ("thu" . "4")
    ("fri" . "5") ("sat" . "6"))
  "Mapping of day-of-week name abbreviations to numbers (0=Sunday).")

;; --- [Date Calculation Helper Functions] ---

(defun org-repeat-by-cron--substitute-aliases (field-str alias-map)
  "Replace text aliases in field string with numbers. Case-insensitive.
Example: (org-repeat-by-cron--substitute-aliases \"MON-FRI\" org-repeat-by-cron--dow-aliases) -> \"1-5\""
  (let ((result (downcase field-str)))
    (dolist (pair alias-map result)
      (let ((alias (car pair))
            (number-str (cdr pair)))
        ;; Use \b to ensure matching whole words only, avoiding replacing \"mon\" in \"monday\" (if applicable)
        (setq result (replace-regexp-in-string (concat "\\b" alias "\\b")
                                               number-str result))))))
(defun org-repeat-by-cron--get-dow (day month year)
  "Return the day of the week for the given date (0=Sunday, ..., 6=Saturday)."
  (nth 6 (decode-time (encode-time 0 0 0 day month year))))

(defun org-repeat-by-cron--find-nth-dow-of-month (n target-dow month year)
  "Calculate the day of the month for the Nth occurrence of a given day-of-week in a month and year.
Example: (org-repeat-by-cron--find-nth-dow-of-month 5 1 9 2025) -> 29 (2025-09-29, Monday)."
  (let* ((first-dow-of-month (org-repeat-by-cron--get-dow 1 month year))
         (day-offset (if (>= target-dow first-dow-of-month)
                         (- target-dow first-dow-of-month)
                       (+ (- target-dow first-dow-of-month) 7))))
    (+ 1 day-offset (* 7 (1- n)))))

(defun org-repeat-by-cron--find-last-dow-of-month (target-dow month year)
  "Calculate the last occurrence of a given day-of-week in a month and year."
  (let* ((last-day (calendar-last-day-of-month month year))
         (last-dow (org-repeat-by-cron--get-dow last-day month year))
         (day-offset (if (>= last-dow target-dow)
                         (- last-dow target-dow)
                       (+ (- last-dow target-dow) 7))))
    (- last-day day-offset)))

(defun org-repeat-by-cron--find-last-weekday-of-month (month year)
  "Calculate the last weekday (Monday to Friday) of a month and year."
  (let* ((last-day (calendar-last-day-of-month month year))
         (last-dow (org-repeat-by-cron--get-dow last-day month year)))
    (cond
     ((= last-dow 6) (- last-day 1)) ; Saturday -> subtract 1 day, go back to Friday
     ((= last-dow 0) (- last-day 2)) ; Sunday -> subtract 2 days, go back to Friday
     (t last-day))))

(defun org-repeat-by-cron--find-nearest-weekday (target-day month year)
  "Find the nearest weekday to TARGET-DAY without crossing month boundaries."
  (let ((dow (org-repeat-by-cron--get-dow target-day month year)))
    (cond
     ;; Already a weekday (Monday to Friday)
     ((and (>= dow 1) (<= dow 5))
      target-day)
     ;; Saturday
     ((= dow 6)
      ;; If it's the 1st day, cannot go backward, must advance to the 3rd (Monday)
      (if (= target-day 1)
          (+ target-day 2)
        ;; Otherwise, go back to Friday
        (- target-day 1)))
     ;; Sunday
     ((= dow 0)
      ;; If it's the last day of the month, cannot advance, must go back to Friday
      (if (= target-day (org-repeat-by-cron--days-in-month month year))
          (- target-day 2)
        ;; Otherwise, advance to Monday
        (+ target-day 1))))))

;; --- [Field Expansion and Matching Logic] ---

;; org-repeat-by-cron--expand-field function is the standard field matcher
(defun org-repeat-by-cron--expand-field (field-str min max)
  "Expand the FIELD-STR into a list of numbers."
  ;; * Means all range.
  (if (string= field-str "*") (number-sequence min max)
    ;; Else, prepare numbers under rules.
    (let ((values '()))
      ;; Split by ','
      (dolist (part (split-string field-str ","))
        (cond
         ;; Step
         ((string-match "/\\([0-9]+\\)" part)
          (let* ((step (string-to-number (match-string 1 part)))
                 (base-str (substring part 0 (match-beginning 0)))
                 (base-range (if (string= base-str "*") (number-sequence min max)
                               (let ((r (mapcar #'string-to-number (split-string base-str "-")))) (number-sequence (car r) (cadr r)))))
                 (result '()))
            (dolist (i base-range (nreverse result)) (when (zerop (% (- i min) step)) (push i result)))
            (setq values (append result values))))
         ;; Range
         ((string-match "-" part) (setq values (append (let ((r (mapcar #'string-to-number (split-string part "-")))) (number-sequence (car r) (cadr r))) values)))
         ;; Ignore [0-9]+W , deal with it later
         ((string-match "\\([0-9]+\\)W" part) (push (string-to-number (match-string 1 part)) values))
         ;; Default numbers, save it directly
         (t (push (string-to-number part) values))))
      (sort (cl-delete-duplicates values) '<))))

(defun org-repeat-by-cron--day-fields-match-p (dom-rule dow-rule day month year &optional day-and)
  "Dynamically check if a date matches the day-of-month (DOM) and day-of-week (DOW) rules, supports L, #, 15W, and 7=Sun."
  (let ((dow (org-repeat-by-cron--get-dow day month year)) dom-match-p dow-match-p)
    ;; 1. Evaluate Day-of-Month (DOM) rule
    (cond
     ;; The LAST rule
     ((string= dom-rule "L")
      (setq dom-match-p (= day (calendar-last-day-of-month month year))))
     ;; The LAST WEEKDAY rule
     ((string= dom-rule "LW") (setq dom-match-p (= day (org-repeat-by-cron--find-last-weekday-of-month month year))))
     ;; The NEAREST WEEKDAY rule
     ((string-match "\\([0-9]+\\)W" dom-rule)
      (let ((target-day (string-to-number (match-string 1 dom-rule))))
        (setq dom-match-p (= day (org-repeat-by-cron--find-nearest-weekday target-day month year)))))
     ;; Default rule
     (t (setq dom-match-p (member day (org-repeat-by-cron--expand-field dom-rule 1 (calendar-last-day-of-month month year))))))
    
    ;; 2. Evaluate Day-of-Week (DOW) rule with comma-separated OR conditions
    (setq dow-match-p
          ;; Check if any of the comma-separated rules match
          (cl-some
           (lambda (part)
             (cond
              ;; Format: L5 or L7 -> last Friday/Sunday
              ((string-match "L\\([0-7]\\)" part)
               (let* ((target-dow (string-to-number (match-string 1 part)))
                      ;; If it's 7, convert to 0 (Sunday)
                      (final-dow (if (= target-dow 7) 0 target-dow)))
                 (= day (org-repeat-by-cron--find-last-dow-of-month final-dow month year))))
              ;; Format: 5#3 or 7#3 -> 3rd Friday/Sunday
              ((string-match "\\([0-7]\\)#\\([1-5]\\)" part)
               (let* ((target-dow (string-to-number (match-string 1 part)))
                      (n (string-to-number (match-string 2 part)))
                      ;; If it's 7, convert to 0 (Sunday)
                      (final-dow (if (= target-dow 7) 0 target-dow)))
                 (= day (org-repeat-by-cron--find-nth-dow-of-month n final-dow month year))))
              ;; Standard format (this part already handles 7 correctly)
              (t (member dow
                         (let ((d (org-repeat-by-cron--expand-field part 0 7)))
                           (if (member 7 d) (sort (cl-delete-duplicates (cons 0 (remq 7 d))) '<) d))))))
           (split-string dow-rule ",")))

    ;; 3. Combine results
    (if (and (not (string= dom-rule "*")) (not (string= dow-rule "*")))
        (if day-and
            (and dom-match-p dow-match-p)
          (or dom-match-p dow-match-p))
      (and dom-match-p dow-match-p))))


;; --- [Main Function] ---

(defun org-repeat-by-cron-next-time (cron-string start-time &optional day-and)
  "Efficiently find the next time matching CRON-STRING starting from START-TIME.
Supports standard cron, 'L', '#', 'LW', '15W', and text abbreviations for months and days of the week.
DAY-AND is used to control how cron-parser handles 'day-of-month' and 'day-of-week' entries. Default option is the cron behaviour, which connects those values using 'OR'. If the switch is set to True, the values are connected using 'AND'. "
  (let* ((cron-parts (split-string cron-string "[ \t]+" t))
         (minute-rule (nth 0 cron-parts))
         (hour-rule (nth 1 cron-parts))
         (dom-rule (nth 2 cron-parts))
         ;; Convert abbreviations to numbers
         (month-rule (org-repeat-by-cron--substitute-aliases (nth 3 cron-parts) org-repeat-by-cron--month-aliases))
         (dow-rule (org-repeat-by-cron--substitute-aliases (nth 4 cron-parts) org-repeat-by-cron--dow-aliases))
         (end-time-year 2099)
         ;; Start searching from the next minute
         (time (time-add start-time 60))
         (decoded-time (decode-time time)))
    ;; Check number of CRON parameters
    (when (/= (length cron-parts) 5) (error "Cron string must have 5 fields"))
    ;; Set seconds field of Emacs time to 0
    (setf (nth 0 decoded-time) 0)
    (cl-block finder
      (let ((start-year (nth 5 decoded-time)) (start-mon  (nth 4 decoded-time))
            (start-day  (nth 3 decoded-time)) (start-hour (nth 2 decoded-time))
            (start-min  (nth 1 decoded-time)))
        ;; Iterate by year
        (cl-loop for year from start-year to end-time-year do
                 ;; Iterate through each month that matches the condition
                 (dolist (month (org-repeat-by-cron--expand-field month-rule 1 12))
                   ;; If this month is greater than or equal to the starting month
                   (when (>= month (if (= year start-year) start-mon 1))
                     ;; Iterate through days of the month, starting from start day if it's the starting month
                     (cl-loop for day from (if (and (= year start-year) (= month start-mon)) start-day 1)
                              to (calendar-last-day-of-month month year) do
                              ;; When year/month/day satisfy cron rules, check hours
                              (when (org-repeat-by-cron--day-fields-match-p dom-rule dow-rule day month year day-and)
                                (let ((current-hour-start (if (and (= year start-year) (= month start-mon) (= day start-day))
                                                              start-hour 0)))
                                  (dolist (hour (org-repeat-by-cron--expand-field hour-rule 0 23))
                                    ;; When hour satisfies the rule, check minutes
                                    (when (>= hour current-hour-start)
                                      (let ((current-min-start (if (and (= year start-year) (= month start-mon) (= day start-day) (= hour start-hour))
                                                                   start-min 0)))
                                        (dolist (minute (org-repeat-by-cron--expand-field minute-rule 0 59))
                                          (when (>= minute current-min-start)
                                            (cl-return-from finder
                                              (encode-time 0 minute hour day month year)))))))))))))
        nil))))

(defvar org-repeat-by-cron-anchor-prop "REPEAT_ANCHOR"
  "Property holding the last rescheduled timestamp. This serves as an
anchor for rescheduling, independent of the current 'SCHEDULED' or 'DEADLINE'
timestamp. The 'REPEAT_ANCHOR' is updated every
time a rescheduling happened.")

(defcustom org-repeat-by-cron-day-and-prop "REPEAT_DAY_AND"
  "Property holding the `DAY-AND' optional. When 't' ,
 use 'AND' in 'DOM' and 'DOW', otherwise default to 'OR'.
More: https://github.com/pallets-eco/croniter.")

(defcustom org-repeat-by-cron-cron-prop "REPEAT_CRON"
  "Property holding a day-only cron expression (DOM MON DOW), or a
full (5-field) (min hour DOM MON DOW). The CRON expression restricts the
possible dates to reschedule the task.")


(defun org-repeat-by-cron--cron-rule-arity (rule)
  "Return number of fields in RULE (3 or 5), or nil if invalid."
  (let* ((parts (split-string (string-trim rule) "[ \t]+" t))
         (n (length parts)))
    (cond ((= n 3) 3)
          ((= n 5) 5)
          (t nil))))

(defun org-repeat-by-cron--normalize-cron-rule (rule)
  "Normalize RULE into a 5-field cron for croniter.
Accepts 3 fields (DOM MON DOW) or 5 fields (MIN HOUR DOM MON DOW)."
  (let* ((parts (split-string (string-trim rule) "[ \t]+" t)))
    (pcase (length parts)
      (3 (concat "0 0 " (string-trim rule)))
      (5 (string-trim rule))
      (_ (progn
           (message "org-repeat-by-cron: invalid cron rule: %S" rule)
           nil)))))

(defun org-repeat-by-cron--reschedule-use-time-p (anchor-str cron-arity scheduled-str)
  "Return a format string for the rescheduled timestamp.
Choose format based on the first existing timestamp or rule:
1. Else if ANCHOR exists, include time if it has HH:MM.
2. Else if CRON is 5-field, include time.
3. Else if SCHEDULED exists, include time if it has HH:MM.
Otherwise, date only."
  (let ((fmt-time  "%Y-%m-%d %a %H:%M")
        (fmt-date  "%Y-%m-%d %a")
        (time-regexp "[0-9]\\{2\\}:[0-9]\\{2\\}"))
    (cond
     (anchor-str
      (if (string-match-p time-regexp anchor-str)
          fmt-time
        fmt-date))
     ((eq cron-arity 5)
      fmt-time)
     (scheduled-str
      (if (string-match-p time-regexp scheduled-str)
          fmt-time
        fmt-date))
     (t fmt-date))))

(defvar org-repeat-by-cron-deadline-prop "REPEAT_DEADLINE"
  "When 't' , use 'org-deadline', otherwise default to 'org-schedule' ")

;;;###autoload
(defun org-repeat-by-cron-on-done ()
  "When a heading is DONE, reschedule it per RESCHEDULE_CRON."
  (when (org-entry-is-done-p)
    (save-excursion
      (org-back-to-heading t)
      (let* ((day-and-str   (org-entry-get (point) org-repeat-by-cron-day-and-prop nil))
             (day-and-p     (if (string= day-and-str "t")
                                 t
                               nil))
             (cron-str       (org-entry-get (point) org-repeat-by-cron-cron-prop nil))
             (deadline-str (org-entry-get (point) org-repeat-by-cron-deadline-prop nil))
             (resched-func (if (string= deadline-str "t")
                                'org-deadline
                             'org-schedule))
             (has-cron       (and cron-str     (> (length (string-trim cron-str))     0)))
             (repeat-time     (if (string= deadline-str "t")
                                 (org-get-deadline-time (point))
                               (org-get-scheduled-time (point))))
             (anchor-str     (org-entry-get (point) org-repeat-by-cron-anchor-prop nil))
             (anchor-time    (when (and anchor-str (> (length (string-trim anchor-str)) 0))
                               (org-time-string-to-time anchor-str)))
             (base-time           (or anchor-time repeat-time (current-time)))
             (cron-arity          (when has-cron     (org-repeat-by-cron--cron-rule-arity cron-str)))
             (norm-cron      (when cron-arity        (org-repeat-by-cron--normalize-cron-rule cron-str)))
             (resched-str  (if (string= deadline-str "t")
                               (org-entry-get (point) "DEADLINE")
                               (org-entry-get (point) "SCHEDULED")))
             (fmt            (org-repeat-by-cron--reschedule-use-time-p
                              anchor-str cron-arity resched-str)))
        (when has-cron
          (when (and has-cron (null cron-arity))
              (message "[repeat] invalid cron rule, skipping"))
          (when-let* ((bigger-time (if (time-less-p base-time nil) (current-time)
                                     base-time))
                      (next (org-repeat-by-cron-next-time norm-cron base-time day-and-p)))
            (when next
              (funcall resched-func nil (format-time-string fmt next))
              ;; When using deadline, we need to manually clear SHCEDULED timestamp
              (when (string= deadline-str "t") (org-schedule '(4)))
              (org-entry-put (point) org-repeat-by-cron-anchor-prop
                             (format-time-string fmt next))
              (org-todo 'todo)
              (message "[repeat] repeated to %s%s"
                       (format-time-string fmt next)
                       (if has-cron ", reset state")))))))))

(add-hook 'org-after-todo-state-change-hook #'org-repeat-by-cron-on-done)

(provide 'org-repeat-by-cron)

;;; org-repeat-by-cron.el ends here

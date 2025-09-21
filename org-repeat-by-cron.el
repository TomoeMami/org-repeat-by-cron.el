;;; org-repeat-by-cron.el --- An Org mode task repeater based on Cron expressions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 TomoeMami

;; Author: TomoeMami <trembleafterme@outlook.com>
;; Created: 2025.09.09

;; Keywords: calendar
;; URL: https://github.com/TomoeMami/org-repeat-by-cron.el

;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; An Org mode task repeater based on Cron expressions
;; Modified from https://github.com/Raemi/org-reschedule-by-rule.
;; Key Differences:
;; - Uses a cron parser implemented in pure Elisp, with
;;   no dependency on the Python croniter package.
;; - Replaces the INTERVAL property with a DAY_AND property.
;; - Supports toggling between SCHEDULED and DEADLINE timestamps.
;; 
;; org-repeat-by-cron.el is a lightweight extension for Emacs Org
  ;; mode that allows you to repeat tasks based on powerful Cron expressions.
;; Standard Org mode repeaters (like +1d , ++1w ) are based on
;; the current SCHEDULED or DEADLINE timestamp. In contrast, this
;; tool provides a repetition method based on absolute time rules.
;; You can easily set a task to repeat \"on the last Friday of
;; every month\" or \"on the first Monday of each quarter\"
;; without manual date calculations.
;; A core advantage of this tool is its pure Elisp implementation,
;; which does not rely on any external programs (like Python's
;; croniter library), ensuring it works out-of-the-box in Emacs environment.
;; 
;; Installation
;; - Download =org-repeat-by-cron.el= and place it in your Emacs load-path.
;; - Add the following code to your Emacs configuration file (e.g., init.el ):
;;   #+begin_src elisp
;; (require 'org-repeat-by-cron)
;;   #+end_src
;; 
;; Using use-package
;; 
;; If you use use-package , the configuration is more concise:
;; #+begin_src elisp
;; (use-package org-repeat-by-cron
;;   :ensure nil  ; If the file is already in your load-path
;;   :load-path "/path/to/your/lisp/directory/")
;; #+end_src
;; 
;; Tip: You should not use org-repeat-by-cron and the built-in
;; Org repeater cookie (e.g., +1w) on the same task.

;;; Code:

(require 'cl-lib)
(require 'org)

(defgroup org-repeat-by-cron nil
  "Customizations for org-repeat-by-cron-mode."
  :group 'org)

(defconst org-repeat-by-cron--month-aliases
  '(("jan" . "1") ("feb" . "2") ("mar" . "3") ("apr" . "4") ("may" . "5")
    ("jun" . "6") ("jul" . "7") ("aug" . "8") ("sep" . "9") ("oct" . "10")
    ("nov" . "11") ("dec" . "12"))
  "Mapping of month name abbreviations to numbers.")

(defconst org-repeat-by-cron--dow-aliases
  '(("sun" . "0") ("mon" . "1") ("tue" . "2") ("wed" . "3") ("thu" . "4")
    ("fri" . "5") ("sat" . "6"))
  "Mapping of day-of-week name abbreviations to numbers (0=Sunday).")

(defcustom org-repeat-by-cron-anchor-prop "REPEAT_ANCHOR"
  "Name of the Org property for the cron repetition anchor timestamp.

This variable holds the string used as the property key to store
the base timestamp for cron-based repetitions.  When a repeating
entry is marked as done, the function `org-repeat-by-cron-on-done'
looks for this property to determine the starting point for
calculating the next occurrence.  The property's value is then
updated to the new scheduled time."
  :group 'org-repeat-by-cron
  :type 'string)

(defcustom org-repeat-by-cron-day-and-prop "REPEAT_DAY_AND"
  "Name of the Org property to control the day-of-month/day-of-week logic.

This variable holds the string used as the property key to specify
how the day-of-month and day-of-week fields of a cron rule are
combined.  The function `org-repeat-by-cron-on-done' checks for
this property in the Org entry.

If this property's value is set to the string \"t\", a date must
satisfy both the day-of-month and day-of-week rules (logical AND).
Otherwise, a date matches if it satisfies either rule (logical OR)."
  :group 'org-repeat-by-cron
  :type 'string)

(defcustom  org-repeat-by-cron-cron-prop "REPEAT_CRON"
  "Name of the Org property for the cron repetition rule.

This variable holds the string used as the property key to store
the cron repetition rule itself.  The function
`org-repeat-by-cron-on-done' looks for an Org entry property
with this key.  The value associated with this property must be a
valid 3- or 5-field cron string."
  :group 'org-repeat-by-cron
  :type 'string)

(defcustom org-repeat-by-cron-deadline-prop "REPEAT_DEADLINE"
  "Name of the Org property to trigger deadline-based repetition.

This variable holds the string used as a property key.  When the
function `org-repeat-by-cron-on-done' processes a completed entry,
it checks for a property with this key.  If the property exists
and its value is the string \"t\", the function will update the
entry's `DEADLINE` timestamp to the next calculated time.  In
this case, any existing `SCHEDULED` timestamp on the entry will be
removed."
  :group 'org-repeat-by-cron
  :type 'string)

;; --- [Date Calculation Helper Functions] ---

(defun org-repeat-by-cron--substitute-aliases (field-str alias-map)
  "Replace textual aliases in FIELD-STR with numeric equivalents from ALIAS-MAP.

This function performs a case-insensitive, whole-word replacement of
aliases found in FIELD-STR.  Each alias is looked up as a key in
ALIAS-MAP, which should be an association list of the form
\(ALIAS . NUMBER-STRING\), and is replaced by its corresponding
value.

For example, if ALIAS-MAP maps \"mon\" to \"1\", this function
would transform \"MON-FRI\" to \"1-5\". The function returns
the transformed string."
  (let ((result (downcase field-str)))
    (dolist (pair alias-map result)
      (let ((alias (car pair))
            (number-str (cdr pair)))
        ;; Use \b to ensure matching whole words only.
        (setq result (replace-regexp-in-string (concat "\\b" alias "\\b")
                                               number-str result))))))

(defun org-repeat-by-cron--get-dow (day month year)
  "Return the day of the week for the date.

Specified by DAY, MONTH, and YEAR.  The returned value is an
integer from 0,7 (Sunday) to 6 (Saturday)."
  (nth 6 (decode-time (encode-time 0 0 0 day month year))))

(defun org-repeat-by-cron--find-nth-dow-of-month (n target-dow month year)
  "Return the day of the month for the Nth TARGET-DOW in MONTH of YEAR.

The argument N specifies the occurrence; for example, 1 for the
first or 5 for the fifth.
TARGET-DOW is an integer from 0 to 7 representing the day of
the week, where 0,7 is Sunday, 1 is Monday, and so on.

For example, to find the fifth (5) Monday (1) of
September (9) in the year (2025), the function
returns 29."
  (let* ((first-dow-of-month (org-repeat-by-cron--get-dow 1 month year))
         (day-offset (if (>= target-dow first-dow-of-month)
                         (- target-dow first-dow-of-month)
                       (+ (- target-dow first-dow-of-month) 7))))
    (+ 1 day-offset (* 7 (1- n)))))

(defun org-repeat-by-cron--find-last-dow-of-month (target-dow month year)
  "Return the day of the month for the last TARGET-DOW in MONTH of YEAR.

TARGET-DOW is an integer from 0 to 7 representing the day of the
week, where 0,7 is Sunday, 1 is Monday, and so on.

For example, to find the last Sunday of September in the year 2025,
 the functionreturns 28."
  (let* ((last-day (calendar-last-day-of-month month year))
         (last-dow (org-repeat-by-cron--get-dow last-day month year))
         (day-offset (if (>= last-dow target-dow)
                         (- last-dow target-dow)
                       (+ (- last-dow target-dow) 7))))
    (- last-day day-offset)))

(defun org-repeat-by-cron--find-last-weekday-of-month (month year)
  "Return the day of the month for the last weekday of MONTH and YEAR.

A weekday is considered any day from Monday to Friday.  If the
last calendar day of the month falls on a Saturday or Sunday, this
function returns the day number of the preceding Friday.
Otherwise, it returns the last day of the month."
  (let* ((last-day (calendar-last-day-of-month month year))
         (last-dow (org-repeat-by-cron--get-dow last-day month year)))
    (cond
     ((= last-dow 6) (- last-day 1)) ; Saturday, go back to Friday
     ((= last-dow 0) (- last-day 2)) ; Sunday, go back to Friday
     (t last-day))))

(defun org-repeat-by-cron--find-nearest-weekday (target-day month year)
  "Return the nearest weekday to TARGET-DAY within the given MONTH and YEAR.

A weekday is any day from Monday to Friday.  If TARGET-DAY is
already a weekday, it is returned unchanged.  If TARGET-DAY is a
Saturday or a Sunday, this function returns the day of the month
of the closest Friday or Monday.

The key constraint is that the returned day will not cross month
boundaries.  For example, if TARGET-DAY is Saturday the 1st,
the function returns 3 (the following Monday), because moving to
the preceding Friday would enter the previous month.  Conversely,
if TARGET-DAY is a Sunday on the last day of the month, it
returns the date of the preceding Friday."
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
      (if (= target-day (calendar-last-day-of-month month year))
          (- target-day 2)
        ;; Otherwise, advance to Monday
        (+ target-day 1))))))

;; --- [Field Expansion and Matching Logic] ---

;; org-repeat-by-cron--expand-field function is the standard field matcher
(defun org-repeat-by-cron--expand-field (field-str min max)
  "Expand the cron-style FIELD-STR into a sorted list of numbers from MIN to MAX.

This function parses FIELD-STR, which uses a syntax similar to
cron time fields, and returns a sorted list of unique integers
that fall within the inclusive range defined by MIN and MAX.

The function supports the following formats within FIELD-STR:
- `*`: A wildcard representing the full range from MIN to MAX.
- `1,5,10`: A comma-separated list of individual values.
- `0-10`: A hyphen-separated, inclusive range of values.
- `*/5`: A step value applied to the full range.  It selects every
  5th value, starting from MIN.
- `10-30/5`: A step value applied to a specific range.

These formats can be combined, for example, \"1-5,20-30/2\".

Additionally, the function recognizes the \"W\" suffix (e.g., \"15W\"),
which in some cron dialects means \"nearest weekday\".  This
function will parse and include the numeric value (15) in the
list, but it does not implement the logic for finding the
nearest weekday.  That responsibility is left to the caller."
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
            (dolist (i base-range (setq result (nreverse result))) (when (zerop (% (- i min) step)) (push i result)))
            (setq values (append result values))))
         ;; Range
         ((string-match "-" part) (setq values (append (let ((r (mapcar #'string-to-number (split-string part "-")))) (number-sequence (car r) (cadr r))) values)))
         ;; Ignore [0-9]+W , deal with it later
         ((string-match "\\([0-9]+\\)W" part) (push (string-to-number (match-string 1 part)) values))
         ;; Default numbers, save it directly
         (t (push (string-to-number part) values))))
      (sort (cl-delete-duplicates values) '<))))

(defun org-repeat-by-cron--day-fields-match-p (dom-rule dow-rule day month year &optional day-and)
  "Return t if the date DAY, MONTH, YEAR matches DOM-RULE and DOW-RULE.

This predicate evaluates if a specific date matches the combined
day-of-month and day-of-week cron rules.

The logic for combining the rules is as follows:
- If either DOM-RULE or DOW-RULE is a wildcard (\"*\"), a date
  matches only if it satisfies both rules (AND condition).
- If both rules are specific (not \"*\"), their results are
  combined with OR if DAY-AND is nil (by default),
  otherwise AND if DAY-AND is t

The DOM-RULE supports several special formats:
- \"L\": The last day of the month.
- \"LW\": The last weekday (Monday-Friday) of the month.
- \"15W\": The nearest weekday to the 15th of the month.

The DOW-RULE also supports special formats, which can be part of
a comma-separated list:
- \"L5\": The last Friday of the month.  Both 0 and 7 represent Sunday.
- \"5#3\": The third Friday of the month."
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
  "Return the next scheduled time after START-TIME that matches CRON-STRING.

The function parses the five-field CRON-STRING to find the next valid
occurrence, starting the search from the minute immediately after
START-TIME.  It returns the result as an Emacs time value, or nil
if no match is found before the year 2099.

CRON-STRING must contain five space-separated fields:
1. Minute (0-59)
2. Hour (0-23)
3. Day of Month (1-31, with special characters like \"L\" or \"W\")
4. Month (1-12 or textual aliases like \"JAN\")
5. Day of Week (0-7 or aliases like \"MON\"; 0 and 7 are Sunday)

The optional argument DAY-AND determines how the day-of-month and
day-of-week fields are combined when both are restricted(combined
with comma \",\"). If DAY-AND is nil (by default), a date need only
satisfy one of the conditions (OR). If it is t, a date must
satisfy both conditions (AND)."
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

(defun org-repeat-by-cron--cron-rule-arity (rule)
  "Return the number of fields in the cron string RULE.

The function returns 3 if RULE contains three space-separated fields,
and 5 if it contains five.  In all other cases, it returns nil.
Whitespace at the beginning or end of RULE is ignored."
  (let* ((parts (split-string (string-trim rule) "[ \t]+" t))
         (n (length parts)))
    (cond ((= n 3) 3)
          ((= n 5) 5)
          (t nil))))

(defun org-repeat-by-cron--normalize-cron-rule (rule)
  "Normalize the cron RULE string to a 5-field format.

If RULE contains three fields (day-of-month, month, day-of-week),
expand it by prepending \"0 0\" for the minute and hour.  If RULE
already contains five fields, return it as is.  In both valid
cases, leading and trailing whitespace is removed.

If RULE has any other number of fields, this function posts an
error message and returns nil."
  (let* ((parts (split-string (string-trim rule) "[ \t]+" t)))
    (pcase (length parts)
      (3 (concat "0 0 " (string-trim rule)))
      (5 (string-trim rule))
      (_ (progn
           (message "org-repeat-by-cron: invalid cron rule: %S" rule)
           nil)))))

(defun org-repeat-by-cron--reschedule-use-time-p (anchor-str cron-arity scheduled-str)
  "Return the appropriate `format-time-string' format for a rescheduled timestamp.

The function determines whether the new timestamp should include a
time component based on ANCHOR-STR, CRON-ARITY, and SCHEDULED-STR.

A format string including time (\"%Y-%m-%d %a %H:%M\") is returned
if any of the following conditions are met, in order of precedence:
1.  ANCHOR-STR is a non-nil string containing a time (e.g., \"HH:MM\").
2.  CRON-ARITY is 5, indicating the cron rule specifies a time.
3.  SCHEDULED-STR is a non-nil string containing a time.

If none of these conditions are true, the function returns a
date-only format string (\"%Y-%m-%d %a\")."
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
     ((eq cron-arity 3)
      fmt-date)
     (scheduled-str
      (if (string-match-p time-regexp scheduled-str)
          fmt-time
        fmt-date))
     (t fmt-date))))

(defun org-repeat-by-cron-on-done ()
  "Reschedule an Org entry using a cron rule when it is marked DONE.

This function is intended to be called from the hook
`org-after-todo-state-change-hook'.  When an entry is marked with
a DONE state, this function calculates the next scheduled time
based on a cron rule and updates the entry accordingly.

The behavior is controlled by properties within the entry's drawer:
- The property stored in `org-repeat-by-cron-cron-prop' must contain
  the cron string.  This is required.
- The property in `org-repeat-by-cron-anchor-prop' stores the
  base time for calculating the next occurrence.  This function
  automatically updates this property to the new time after a
  reschedule.
- If the property in `org-repeat-by-cron-deadline-prop' is \"t\",
  this function reschedules the `DEADLINE` timestamp and clears
  the SCHEDULED one.  Otherwise, it reschedules SCHEDULED.
- If the property in `org-repeat-by-cron-day-and-prop' is \"t\",
  the day-of-month and day-of-week rules are combined with a
  logical AND; otherwise, they are combined with OR.

The base time for the calculation is chosen in the following order
of precedence: the anchor property's time, the existing SCHEDULED
or DEADLINE time, and finally the current time.

Upon a successful reschedule, the entry's TODO state is reset to
the first non-done state (TODO by default)."
  (when (org-entry-is-done-p)
    (save-excursion
      (org-back-to-heading t)
      (let* ((day-and-str   (org-entry-get (point) org-repeat-by-cron-day-and-prop nil))
             (day-and-p     (if (string= day-and-str "t") t nil))
             (cron-str       (org-entry-get (point) org-repeat-by-cron-cron-prop nil))
             (deadline-str (org-entry-get (point) org-repeat-by-cron-deadline-prop nil))
             (resched-func (if (string= deadline-str "t") #'org-deadline #'org-schedule))
             (has-cron       (and cron-str     (> (length (string-trim cron-str))     0)))
             (repeat-time     (if (string= deadline-str "t")  (org-get-deadline-time (point)) (org-get-scheduled-time (point))))
             (anchor-str     (org-entry-get (point) org-repeat-by-cron-anchor-prop nil))
             (anchor-time    (when (and anchor-str (> (length (string-trim anchor-str)) 0))
                               (org-time-string-to-time anchor-str)))
             (base-time           (or anchor-time repeat-time (current-time)))
             (cron-arity          (when has-cron     (org-repeat-by-cron--cron-rule-arity cron-str)))
             (norm-cron      (when cron-arity        (org-repeat-by-cron--normalize-cron-rule cron-str)))
             (resched-str  (org-entry-get (point) (if (string= deadline-str "t") "DEADLINE" "SCHEDULED")))
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
;;;###autoload
(define-minor-mode global-org-repeat-by-cron-mode
  "A global minor mode globally enable org-repeat-by-cron."
  :init-value nil
  :global t
  :group 'org-repeat-by-cron
  (if global-org-repeat-by-cron-mode
      (add-hook 'org-after-todo-state-change-hook #'org-repeat-by-cron-on-done)
    (remove-hook 'org-after-todo-state-change-hook #'org-repeat-by-cron-on-done)))

(provide 'org-repeat-by-cron)

;;; org-repeat-by-cron.el ends here

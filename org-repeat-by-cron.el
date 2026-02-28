;;; org-repeat-by-cron.el --- An Org mode task repeater based on Cron expressions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 TomoeMami

;; Author: TomoeMami <trembleafterme@outlook.com>
;; Created: 2025.09.09

;; Keywords: calendar
;; URL: https://github.com/TomoeMami/org-repeat-by-cron.el

;; Version: 1.1.4
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
;; 
;; Modified from https://github.com/Raemi/org-reschedule-by-rule.
;; 
;; Key Differences:
;; - Uses a cron parser implemented in pure Elisp, with
;;   no dependency on the Python croniter package.
;; - Replaces the INTERVAL property with a DAY_AND property.
;; - Supports toggling between SCHEDULED and DEADLINE timestamps.
;; - Fully compatible with org-habit.
;; 
;; org-repeat-by-cron.el is a lightweight extension for Emacs Org
;; mode that allows you to repeat tasks using the power of Cron expressions.
;; Standard Org mode repeaters (like +1d , ++1w ) are relative to
;; the current SCHEDULED or DEADLINE timestamp. In contrast, this
;; tool provides repetition based on absolute time rules. You can
;; easily set a task to repeat "on the last Friday of every month"
;; or "on the first Monday of each quarter" without manual date
;; calculations.
;; 
;; A core advantage is its pure Elisp implementation, ensuring it
;; works out-of-the-box in any Emacs environment without external
;; dependencies.
;; 
;; Installation
;; Install via Melpa :
;; #+begin_src elisp
;; (use-package org-repeat-by-cron
;;   :ensure t
;;   :config
;;   (global-org-repeat-by-cron-mode))
;; #+end_src
;; 
;; Usage
;; 
;; To make an Org task repeat according to a Cron rule,
;; simply add the =REPEAT_CRON= property to its =PROPERTIES= drawer.
;;
;; Tip: You do not need to wrap the =REPEAT_CRON= value in quotes.
;; 
;; Example
;; 
;; Suppose we have a weekly course:
;; 
;; #+begin_src org
;; ,* TODO Weekend Course
;; :PROPERTIES:
;; :REPEAT_CRON: * * SAT,SUN
;; :END:
;; #+end_src
;; 
;; When marked as =DONE=, it will automatically be rescheduled
;; to the next matching date. For example, if today is Dec 9, 2025,
;; it will be scheduled for the coming Saturday (Dec 13, 2025):
;; 
;; #+begin_src org
;; ,* TODO Weekend Course
;; SCHEDULED: <2025-12-13 Sat>
;; :PROPERTIES:
;; :REPEAT_CRON: * * SAT,SUN
;; :REPEAT_ANCHOR: 2025-12-13 Sat
;; :END:
;; #+end_src
;; 
;; Marking it =DONE= again will calculate the next point based
;; on the =REPEAT_ANCHOR= and the current time:
;; 
;; #+begin_src org
;; ,* TODO Weekend Course
;; SCHEDULED: <2025-12-14 Sun>
;; :PROPERTIES:
;; :REPEAT_CRON: * * SAT,SUN
;; :REPEAT_ANCHOR: 2025-12-14 Sun
;; :END:
;; #+end_src
;; 
;; Tip1: If you do not want to specify a specific hour/minute,
;; use the *3-field shorthand* (discussed below).
;; Tip2: If you use org-repeat-by-cron with the built-in Org repeater
;; cookie (e.g., +1w) on the same task, the build-in cookie will
;; be taken over and preserved, so you can use this package with
;; org-habit.
;;
;; Check README for more usage: https://github.com/TomoeMami/org-repeat-by-cron.el
;; 
;;; Code:

(require 'cl-lib)
(require 'org)

(defgroup org-repeat-by-cron nil
  "Cron-style task repetition for Org mode.

This group contains options for a system that allows Org tasks to repeat
according to complex cron-style schedules instead of simple intervals."
  :group 'org)

(defconst org-repeat-by-cron--month-aliases
  '(("jan" . "1") ("feb" . "2") ("mar" . "3") ("apr" . "4") ("may" . "5")
    ("jun" . "6") ("jul" . "7") ("aug" . "8") ("sep" . "9") ("oct" . "10")
    ("nov" . "11") ("dec" . "12"))
  "Alist mapping month name abbreviations to their numeric strings.")

(defconst org-repeat-by-cron--dow-aliases
  '(("sun" . "0") ("mon" . "1") ("tue" . "2") ("wed" . "3") ("thu" . "4")
    ("fri" . "5") ("sat" . "6"))
  "Alist mapping day-of-week abbreviations to their numeric strings.")

(defcustom org-repeat-by-cron-anchor-prop "REPEAT_ANCHOR"
  "Org property name used to store the last calculated repeat timestamp.

This property acts as the reference point for calculating the next
repetition occurrence to ensure consistency when a task is completed."
  :group 'org-repeat-by-cron
  :type 'string)

(defcustom org-repeat-by-cron-day-and-prop "REPEAT_DAY_AND"
  "Org property name that determines the logic for day matching.

If this property is set to \"t\" in an entry, the next occurrence
must satisfy both the day-of-month and the day-of-week rules.
Otherwise, it satisfies either rule (standard cron behavior)."
  :group 'org-repeat-by-cron
  :type 'string)

(defcustom  org-repeat-by-cron-cron-prop "REPEAT_CRON"
  "Org property name used to store the cron expression for a task.

The value should be a standard 5-field cron string or a 3-field
string (Day Month Day-of-Week) for daily-level precision."
  :group 'org-repeat-by-cron
  :type 'string)

(defcustom org-repeat-by-cron-deadline-prop "REPEAT_DEADLINE"
  "Org property name defining how deadlines are rescheduled via cron.

In an Org entry, the property named by this variable determines
the logic used to move the deadline forward when a task is
marked DONE.

If the property named by this variable is set to \"t\",
the next deadline is calculated using the primary cron expression
from `org-repeat-by-cron-cron-prop'. This calculation uses the
entry's current deadline as the anchor time instead of its
scheduled time. Additionally, the existing `SCHEDULED' timestamp is
cleared when the task is marked DONE.

Setting the property value to a specific cron expression (e.g.,
\"0 9 1 * *\") decouples the deadline from the schedule. The
deadline will be recalculated according to its own rule,
regardless of when the task is scheduled to start."
  :group 'org-repeat-by-cron
  :type 'string)

(defcustom org-repeat-by-cron-deadline-anchor-prop "REPEAT_DEADLINE_ANCHOR"
  "Org property name for storing the last calculated deadline time.

This property is automatically created and used only when the property
specified by `org-repeat-by-cron-deadline-prop' contains a specific
cron expression. If that property is set to the string \"t\", the
deadline is recalculated using the primary cron rule instead, and
this anchor property will not be created or updated."
  :group 'org-repeat-by-cron
  :type 'string)

(defcustom org-repeat-by-cron-max-search-year 50
  "Maximum number of years to look ahead for a matching date.

This prevents infinite loops if a cron expression describes a date
that is impossible or extremely far in the future."
  :group 'org-repeat-by-cron
  :type 'integer)

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
  "Return the day of the week for DAY, MONTH, and YEAR.

The result is an integer between 0 and 6, where 0 represents
Sunday, 1 represents Monday, and so on.

The arguments DAY, MONTH, and YEAR must be integers. MONTH
should be in the range 1--12."
  (calendar-day-of-week (list month day year)))

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
  (if (string= field-str "*")
      (number-sequence min max)
    ;; Else, prepare numbers under rules.
    (let (values)
      ;; Split by ','
      (dolist (part (split-string field-str ","))
        (cond
         ;; Step
         ((string-match "\\(.*\\)/\\([0-9]+\\)" part)
          (let* ((range-str (match-string 1 part))
                 (step (string-to-number (match-string 2 part)))
                 (range (if (string= range-str "*")
                            (cons min max)
                          (let ((r (split-string range-str "-")))
                            (cons (string-to-number (car r)) (string-to-number (cadr r)))))))
            (cl-loop for i from (car range) to (cdr range) by step
                     do (push i values))))
         ;; Range
         ((string-match "\\([0-9]+\\)-\\([0-9]+\\)" part)
          (cl-loop for i from (string-to-number (match-string 1 part))
                   to (string-to-number (match-string 2 part))
                   do (push i values)))
         ;; Ignore [0-9]+W , deal with it later
         ((string-match "\\([0-9]+\\)W" part)
          (push (string-to-number (match-string 1 part)) values))
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
- \"5L\": The last Friday of the month.  Both 0 and 7 represent Sunday.
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
              ;; Format: 5L or 7L -> last Friday/Sunday
              ((string-match "\\([0-7]\\)L" part)
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
if no match is found within `org-repeat-by-cron-max-search-year'.

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
         (minute-list   (org-repeat-by-cron--expand-field (nth 0 cron-parts) 0 59))
         (hour-list  (org-repeat-by-cron--expand-field (nth 1 cron-parts) 0 23))
         (dom-rule (nth 2 cron-parts))
         ;; Convert abbreviations to numbers
         (month-rule (org-repeat-by-cron--substitute-aliases (nth 3 cron-parts) org-repeat-by-cron--month-aliases))
         (month-list (org-repeat-by-cron--expand-field month-rule 1 12))
         (dow-rule (org-repeat-by-cron--substitute-aliases (nth 4 cron-parts) org-repeat-by-cron--dow-aliases))
         (end-time-year (+ (nth 5 (decode-time)) org-repeat-by-cron-max-search-year))
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
                 (dolist (month month-list)
                   ;; If this month is greater than or equal to the starting month
                   (when (>= month (if (= year start-year) start-mon 1))
                     ;; Iterate through days of the month, starting from start day if it's the starting month
                     (cl-loop for day from (if (and (= year start-year) (= month start-mon)) start-day 1)
                              to (calendar-last-day-of-month month year) do
                              ;; When year/month/day satisfy cron rules, check hours
                              (when (org-repeat-by-cron--day-fields-match-p dom-rule dow-rule day month year day-and)
                                (let ((current-hour-start (if (and (= year start-year) (= month start-mon) (= day start-day))
                                                              start-hour 0)))
                                  (dolist (hour hour-list)
                                    ;; When hour satisfies the rule, check minutes
                                    (when (>= hour current-hour-start)
                                      (let ((current-min-start (if (and (= year start-year) (= month start-mon) (= day start-day) (= hour start-hour))
                                                                   start-min 0)))
                                        (dolist (minute minute-list)
                                          (when (>= minute current-min-start)
                                            (cl-return-from finder
                                              (encode-time 0 minute hour day month year)))))))))))))
        nil))))

(defun org-repeat-by-cron--cron-rule-arity (rule)
  "Return the number of fields in the cron string RULE.

The function returns 3 if RULE contains three space-separated fields,
and 5 if it contains five.  In all other cases, it returns nil.
Whitespace at the beginning or end of RULE is ignored."
  (if rule
      (let* ((parts (split-string (string-trim rule) "[ \t]+" t))
             (n (length parts)))
        (cond ((= n 3) 3)
              ((= n 5) 5)
              (t nil)))))

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
           (message "[Cron-Repeat] Invalid cron rule: %s" rule)
           nil)))))

(defun org-repeat-by-cron--reschedule-use-time-p (anchor-str cron-arity scheduled-str)
  "Return the appropriate `format-time-string' format for a rescheduled timestamp.

The function determines whether the new timestamp should include a
time component based on ANCHOR-STR, CRON-ARITY, and SCHEDULED-STR.

A format string including time (\"%Y-%m-%d %a %H:%M\") is returned
if any of the following conditions are met, in order of precedence:
1.  CRON-ARITY is 5, indicating the cron rule specifies a time.
2.  ANCHOR-STR is a non-nil string containing a time (e.g., \"HH:MM\").
3.  SCHEDULED-STR is a non-nil string containing a time.

If none of these conditions are true, the function returns a
date-only format string (\"%Y-%m-%d %a\")."
  (let ((time-regexp "[0-9]\\{2\\}:[0-9]\\{2\\}"))
    (if (or (eq cron-arity 5)
            (and anchor-str (string-match-p time-regexp anchor-str))
            (and scheduled-str (string-match-p time-regexp scheduled-str)))
        "%Y-%m-%d %a %H:%M"
      "%Y-%m-%d %a")))

(defun org-repeat-by-cron--extract-repeater (ts-str)
  "Extract the Org repeater cookie from timestamp string TS-STR.

Return the match (e.g., \"+1d\", \"++2w\", or \".1m/3d\") if found,
otherwise nil."
  (when (and ts-str
             (string-match org-repeat-re ts-str))
    (match-string 1 ts-str)))

(defvar org-repeat-by-cron--repeater-raw nil
  "Symbol indicating which timestamps contain native Org repeater cookies.

This variable tracks whether SCHEDULED or DEADLINE timestamps
of the current entry had a native repeater (e.g., \"+1d\") before
cron rescheduling. It is used to determine if existing cookies
should be preserved to maintain compatibility with `org-habit'.
Possible values are schedule , deadline, both, or nil.")

(defun org-repeat-by-cron--ensure-repeater ()
  "Ensure the current Org entry has a repeater cookie for cron repetition.

This function is called by `org-after-todo-state-change-hook' to ensure that
entries using `org-repeat-by-cron' are recognized as repeating tasks.
It adds a temporary repeater cookie if none is present, ensuring
`org-trigger-hook' is run and `org-habit' consistency is maintained."
  (save-excursion
    (org-back-to-heading t)
    (let* ((pom (point))
           (cron-str (org-entry-get pom org-repeat-by-cron-cron-prop))
           (cron-arity (org-repeat-by-cron--cron-rule-arity cron-str)))
      (when (and cron-str (not (string-empty-p (string-trim cron-str)))
                 ;; org-state dynamically bound in org.el/org-todo
                 (member org-state org-done-keywords))
        (if (not cron-arity)
            (message "[Cron-Repeat] Invalid cron rule: %s" cron-str)
          (let* ((deadline-prop (org-entry-get pom org-repeat-by-cron-deadline-prop))
                 (process-deadline nil)
                 (process-schedule nil))

            (cond
             ((string= deadline-prop "t")
              (setq process-deadline t))
             ((org-repeat-by-cron--cron-rule-arity deadline-prop)
              (setq process-deadline t)
              (setq process-schedule t))
             (t
              (setq process-schedule t)))
            
            (let ((sched-has-rep nil)
                  (dead-has-rep nil))
              (when process-schedule
                (let* ((ts-str (org-entry-get pom "SCHEDULED"))
                       (has-ts (and ts-str (not (string-empty-p ts-str))))
                       (repeater (and has-ts (org-repeat-by-cron--extract-repeater ts-str))))
                  (cond
                   (repeater
                    (setq sched-has-rep t))
                   ;; a timestamp without repeater，add a temp repeater "+1d"
                   (has-ts
                    (let ((new-ts-str (if (string-match-p "\\([>]\\|\\]\\)$" ts-str)
                                          (replace-regexp-in-string "\\([>]\\|\\]\\)$" " +1d\\1" ts-str)
                                        (concat ts-str " +1d"))))
                      (org-schedule nil new-ts-str)))
                   ;; when no timestamp, create one
                   (t
                    (let* ((arity (org-repeat-by-cron--cron-rule-arity cron-str))
                           (fmt (if (eq arity 5) "%Y-%m-%d %a %H:%M +1d" "%Y-%m-%d %a +1d"))
                           (new-ts-str (format-time-string fmt (current-time))))
                      (org-schedule nil new-ts-str))))))
            (when process-deadline
              (let* ((ts-str (org-entry-get pom "DEADLINE"))
                     (has-ts (and ts-str (not (string-empty-p ts-str))))
                     (repeater (and has-ts (org-repeat-by-cron--extract-repeater ts-str)))
                     (cron-val (if (string= deadline-prop "t") cron-str deadline-prop)))
                (cond
                 (repeater
                  (setq dead-has-rep t))
                 ;; a timestamp without repeater，add a temp repeater "+1d"
                 (has-ts
                  (let ((new-ts-str (if (string-match-p "\\([>]\\|\\]\\)$" ts-str)
                                        (replace-regexp-in-string "\\([>]\\|\\]\\)$" " +1d\\1" ts-str)
                                      (concat ts-str " +1d"))))
                    (org-deadline nil new-ts-str)))
                 ;; when no timestamp, create one
                 (t
                  (let* ((arity (org-repeat-by-cron--cron-rule-arity cron-val))
                         (fmt (if (eq arity 5) "%Y-%m-%d %a %H:%M +1d" "%Y-%m-%d %a +1d"))
                         (new-ts-str (format-time-string fmt (current-time))))
                    (org-deadline nil new-ts-str))))))
            
            ;; 4. set org-repeat-by-cron--repeater-raw
            (setq org-repeat-by-cron--repeater-raw
                  (cond
                   ((and sched-has-rep dead-has-rep) 'both)
                   (sched-has-rep 'schedule)
                   (dead-has-rep 'deadline)
                   (t nil))))))))))


(defun org-repeat-by-cron-on-done (change-plist)
  "Reschedule the Org task at point according to CHANGE-PLIST and cron rules.

This function is intended for `org-trigger-hook'. It reads the cron rule from
 the property named by the variable `org-repeat-by-cron-cron-prop'.
It then calculates the next occurrence after the current time or anchor time
and updates the SCHEDULED or DEADLINE timestamps. The anchor property
is also updated to ensure consistent calculation for the next repetition."
  (let* ((from-str (format "%s" (plist-get change-plist :from)))
         (to-str (format "%s" (plist-get change-plist :to)))
         (pom (point))
         (cron-str (org-entry-get pom org-repeat-by-cron-cron-prop)))
    (when (and cron-str
               (not (string-empty-p (string-trim cron-str)))
               (not (member from-str org-done-keywords))
               (member to-str org-done-keywords))
      (save-excursion
        (org-back-to-heading t)
        (let ((cron-arity (org-repeat-by-cron--cron-rule-arity cron-str)))
          (if (not cron-arity)
              (message "[Cron-Repeat] Invalid cron rule: %s" cron-str)
            (let* ((deadline-prop (org-entry-get pom org-repeat-by-cron-deadline-prop))
                   (process-deadline nil)
                   (process-schedule nil)
                   (keep-sched (memq org-repeat-by-cron--repeater-raw '(both schedule)))
                   (keep-dead  (memq org-repeat-by-cron--repeater-raw '(both deadline))))
              
              (cond
               ;; REPEAT_DEADLINE is t，only deadline
               ((string= deadline-prop "t")
                (setq process-deadline t))
               ;; REPEAT_DEADLINE as cron ，deal with deadline and schedule
               ((org-repeat-by-cron--cron-rule-arity deadline-prop)
                (setq process-deadline t)
                (setq process-schedule t))
               ;; nil or other, only schedule
               (t
                (setq process-schedule t)))

              (cl-labels ((calc-next-ts (cron-val anchor-prop time-type)
                            "Internal func, calculate the next timestamp, update anchor,
 and return time-string"
                            (let* ((anchor-str (org-entry-get pom anchor-prop))
                                   (day-and-p (string= (org-entry-get pom org-repeat-by-cron-day-and-prop) "t"))
                                   (now (current-time))
                                   (current-ts-str (org-entry-get pom (if (eq time-type 'deadline) "DEADLINE" "SCHEDULED")))
                                   (repeat-time (if (eq time-type 'deadline)
                                                    (org-get-deadline-time pom)
                                                  (org-get-scheduled-time pom)))
                                   (base-time (or (and anchor-str (org-time-string-to-time anchor-str))
                                                  repeat-time
                                                  now))
                                   (next (org-repeat-by-cron-next-time
                                          (org-repeat-by-cron--normalize-cron-rule cron-val)
                                          (if (time-less-p base-time now) now base-time)
                                          day-and-p))
                                   (c-arity (org-repeat-by-cron--cron-rule-arity cron-val))
                                   (fmt (org-repeat-by-cron--reschedule-use-time-p anchor-str c-arity current-ts-str)))
                              (if next
                                  (let ((next-raw (format-time-string fmt next)))
                                    (org-entry-put pom anchor-prop next-raw)
                                    next-raw)
                                (message "[Cron-Repeat] Cannot find valid time before %s"
                                         (+ (nth 5 (decode-time)) org-repeat-by-cron-max-search-year))
                                nil))))

                ;; 2. Schedule
                (when process-schedule
                  (let ((res (calc-next-ts cron-str org-repeat-by-cron-anchor-prop 'schedule)))
                    (when res
                      (message "test repeater %s" res)
                      (unless keep-sched (org-schedule '(4)))
                      (org-schedule nil res)
                      (message "[Cron-Repeat] SCHEDULED repeat to %s" res))))

                ;; 3. Deadline
                (when process-deadline
                  (let* ((is-t (string= deadline-prop "t"))
                         (d-cron (if is-t cron-str deadline-prop))
                         (d-anchor-prop (if is-t org-repeat-by-cron-anchor-prop org-repeat-by-cron-deadline-anchor-prop))
                         (res (calc-next-ts d-cron d-anchor-prop 'deadline)))
                    (when res
                      (unless keep-dead (org-deadline '(4)))
                      (org-deadline nil res)
                      (message "[Cron-Repeat] DEADLINE repeat to %s"  res))))))))))))

;;;###autoload
(define-minor-mode global-org-repeat-by-cron-mode
  "Toggle global cron-style task repetition in Org mode.

When enabled, tasks with a REPEAT_CRON property will automatically
reschedule themselves to the next occurrence matching the cron
expression when marked as DONE. This mode adds functions
to `org-trigger-hook' and `org-after-todo-state-change-hook'."
  :init-value nil
  :global t
  :group 'org-repeat-by-cron
  (if global-org-repeat-by-cron-mode
      (progn
        (add-hook 'org-trigger-hook #'org-repeat-by-cron-on-done)
        (add-hook 'org-after-todo-state-change-hook #'org-repeat-by-cron--ensure-repeater))
    (remove-hook 'org-trigger-hook #'org-repeat-by-cron-on-done)
    (remove-hook 'org-after-todo-state-change-hook #'org-repeat-by-cron--ensure-repeater)))

(provide 'org-repeat-by-cron)
;;; org-repeat-by-cron.el ends here

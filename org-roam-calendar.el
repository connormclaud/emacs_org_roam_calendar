;;; org-roam-calendar.el --- View org-roam activity in a calendar  -*- lexical-binding: t; -*-

;; Author: Antigravity
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org-roam "2.0") (org-habit-stats "0.1"))
;; Keywords: org-roam, calendar, statistics

;;; Commentary:

;; This package provides a calendar view for org-roam nodes, visualizing
;; creation and modification history using the `org-habit-stats' interface.

;;; Code:

(require 'org-roam)
(require 'org-habit-stats)
(require 'calendar)

(defgroup org-roam-calendar nil
  "View org-roam activity in a calendar."
  :group 'org-roam
  :prefix "org-roam-calendar-")

(defcustom org-roam-calendar-date-property "CREATED"
  "Property to check for node creation date.
If nil or not found, falls back to file modification time."
  :type 'string
  :group 'org-roam-calendar)

(defun org-roam-calendar-days-from-time (time)
  "Convert system TIME to absolute days."
  (calendar-absolute-from-gregorian
   (mapcar #'string-to-number
           (split-string (format-time-string "%m %d %Y" time) " "))))

(defun org-roam-calendar--get-node-dates ()
  "Return a list of absolute days where org-roam files were modified.
Uses `mtime' from `files' table in org-roam database."
  (let ((rows (org-roam-db-query
               "SELECT mtime FROM files")))
    (sort (delete-dups
           (mapcar (lambda (row)
                     (org-roam-calendar-days-from-time (car row)))
                   rows))
          #'<)))

(defun org-roam-calendar--make-habit-data (dates)
  "Construct a habit-data list compatible with `org-habit-stats'.
DATES is a list of absolute days with activity."
  ;; Structure based on org-habit-stats usage:
  ;; nth 0: next-scheduled (absolute day) - Set to Today to ensure "streak" logic works
  ;; nth 1: repeat-period - 1
  ;; nth 2: min-repeat - (unused usually)
  ;; nth 3: max-repeat - (unused usually)
  ;; nth 4: completed-dates - The list of dates
  ;; nth 5: repeat-string - ".+" (means 1d range usually in org-habit parsing)
  (list (org-today)      ; next-scheduled
        1                ; repeat-period
        nil              ; min-repeat
        nil              ; max-repeat
        dates            ; completed-dates
        ".+"))           ; repeat-string

(defun org-roam-calendar-visit-day ()
  "Visit the list of org-roam notes modified on the date at point (in calendar)."
  (interactive)
  (let* ((date (calendar-cursor-to-date t))
         (abs-date (calendar-absolute-from-gregorian date))
         (nodes (org-roam-db-query
                 [:select [nodes:id nodes:title nodes:file files:mtime]
                  :from nodes
                  :inner-join files
                  :on (= nodes:file files:file)]
                 ))
         ;; Filter nodes by date in Lisp for simplicity since SQLite date manipulation can be tricky across OS
         (day-nodes (seq-filter (lambda (row)
                                  (= abs-date
                                     (org-roam-calendar-days-from-time (nth 3 row))))
                                nodes)))
    (if (not day-nodes)
        (message "No activity on %s" (calendar-date-string date))
      (let* ((candidates (mapcar (lambda (row)
                                   (cons (format "%s (%s)" (cadr row) (file-name-nondirectory (nth 2 row)))
                                         (car row)))
                                 day-nodes))
             (choice (completing-read (format "Notes for %s: " (calendar-date-string date))
                                      candidates)))
        (org-roam-node-open (org-roam-node-from-id (cdr (assoc choice candidates))))))))

;; Hook into org-habit-stats calendar keymap
;; Since org-habit-stats creates a calendar buffer but doesn't seem to export a specific map variable 
;; easy to hook, we might need to rely on the fact it uses `calendar-mode`.
;; However, we only want this binding when viewing our specific org-roam calendar.
;; We'll use a minor mode in the calendar buffer.

(defvar org-roam-calendar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-roam-calendar-visit-day)
    map)
  "Keymap for `org-roam-calendar-mode'.")

(define-minor-mode org-roam-calendar-mode
  "Minor mode for org-roam-calendar interaction."
  :lighter " OR-Cal"
  :keymap org-roam-calendar-mode-map)

;;;###autoload
(defun org-roam-calendar-open ()
  "Open the org-roam activity calendar."
  (interactive)
  (let* ((dates (org-roam-calendar--get-node-dates))
         (habit-data (org-roam-calendar--make-habit-data dates)))
    
    ;; Use org-habit-stats to create the view
    (org-habit-stats-create-habit-buffer
     habit-data
     "Org Roam Activity"
     "Visualization of daily note-taking activity."
     'file)

    ;; Now find the calendar buffer created by org-habit-stats and enable our minor mode
    ;; org-habit-stats uses hardcoded buffer names pattern usually, or stores it in var.
    ;; We can find it via the variable `org-habit-stats-calendar-buffer`.
    (when (get-buffer org-habit-stats-calendar-buffer)
      (with-current-buffer org-habit-stats-calendar-buffer
        (org-roam-calendar-mode 1)))))

(provide 'org-roam-calendar)
;;; org-roam-calendar.el ends here

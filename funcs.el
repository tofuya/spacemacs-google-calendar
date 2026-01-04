(defun google-calendar--calfw-open-files (files &optional title color)
  "Open calfw calendar for org FILES.
FILES is a list of org file paths."
  (require 'calfw)
  (require 'calfw-org)
  (let ((source
         (calfw-org-create-source
          files
          (or title "Org")
          (or color "green"))))
    (calfw-open-calendar-buffer
     :contents-sources (list source))))

(defun google-calendar-show-file (file)
  "Show calfw calendar for specific org FILE."
  (interactive "fOrg file: ")
  (if (file-exists-p file)
      (google-calendar--calfw-open-files
       (list file)
       (file-name-nondirectory file))
    (user-error "File not found: %s" file)))

(defun google-calendar-show-gcal ()
  "Select org-gcal calendar and open it in calfw."
  (interactive)
  (require 'org-gcal)
  (unless org-gcal-file-alist
    (user-error "org-gcal-file-alist is empty"))
  (let* ((candidates
          (mapcar (lambda (pair)
                    (cons (file-name-nondirectory (cdr pair))
                          (expand-file-name (cdr pair))))
                  org-gcal-file-alist))
         (choice (completing-read "Calendar: " candidates nil t))
         (file (cdr (assoc choice candidates))))
    (google-calendar--calfw-open-files
     (list file)
     choice)))

(defun google-calendar-show-current-buffer ()
  "Show current org buffer in calfw."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not an org buffer"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (google-calendar--calfw-open-files
   (list buffer-file-name)
   (file-name-nondirectory buffer-file-name)))

(defun google-calendar--calfw-open-files (files &optional title color)
  "Open calfw calendar for org FILES.
FILES is a list of org file paths."
  (require 'calfw)
  (require 'calfw-org)
  (let ((source
         (calfw-org-create-source
          files
          (or title "Org")
          (or color "green"))))
    (calfw-open-calendar-buffer
     :contents-sources (list source))))

(defun google-calendar-show-file (file)
  "Show calfw calendar for specific org FILE."
  (interactive "fOrg file: ")
  (if (file-exists-p file)
      (google-calendar--calfw-open-files
       (list file)
       (file-name-nondirectory file))
    (user-error "File not found: %s" file)))

(defun google-calendar-show-gcal ()
  "Select org-gcal calendar and open it in calfw."
  (interactive)
  (require 'org-gcal)
  (unless org-gcal-file-alist
    (user-error "org-gcal-file-alist is empty"))
  (let* ((candidates
          (mapcar (lambda (pair)
                    (cons (file-name-nondirectory (cdr pair))
                          (expand-file-name (cdr pair))))
                  org-gcal-file-alist))
         (choice (completing-read "Calendar: " candidates nil t))
         (file (cdr (assoc choice candidates))))
    (google-calendar--calfw-open-files
     (list file)
     choice)))

(defun google-calendar-define-open-commands ()
  "Define google-calendar-open-<label> commands from `google-calendar-calendars`."
  (dolist (c google-calendar-calendars)
    (let* ((label (plist-get c :label))
           (file-path (plist-get c :file))
           (fn-name (intern (format "google-calendar-open-%s" label))))
      (eval
       `(defun ,fn-name ()
          ,(format "Open %s calendar org file." label)
          (interactive)
          (find-file ,file-path))))))

(defun google-calendar-show-current-buffer ()
  "Show current org buffer in calfw."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not an org buffer"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (google-calendar--calfw-open-files
   (list buffer-file-name)
   (file-name-nondirectory buffer-file-name)))

(defun google-calendar--calendar-alist ()
  "Return alist of (label . calendar-id) from `google-calendar-calendars`."
  (mapcar (lambda (c)
            (cons (plist-get c :label)
                  (plist-get c :calendar-id)))
          google-calendar-calendars))

(defun google-calendar-select-calendar-id ()
  "Prompt user to select a calendar and return its calendar-id."
  (let* ((alist (google-calendar--calendar-alist))
         (choice (completing-read "Calendar: " alist nil t)))
    (cdr (assoc choice alist))))

(defconst google-calendar-post-entry-template
  ":PROPERTIES:
:calendar-id: %s
:org-gcal-managed: org
:END:
:org-gcal:

:END:
"
  "Template for org-gcal entry.")

(defun google-calendar-insert-org-gcal-skeleton ()
  "Insert org-gcal posting skeleton at point."
  (interactive)
  (insert (format google-calendar-post-entry-template
                  (google-calendar-select-calendar-id))))

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
  "Define commands to open calendar org files."
  "Define google-calendar-open-<label> commands from `google-calendar-calendars`."
  (dolist (c google-calendar-calendars)
    (let* ((label (plist-get c :label))
           (file  (plist-get c :file))
           (fn    (intern (format "google-calendar-open-%s" label))))
      (fset fn
            `(lambda ()
               ,(format "Open %s calendar org file." label)
               (interactive)
               (find-file ,file))))))

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

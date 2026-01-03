(defcustom google-calendar-calendars nil
  "Google Calendar definitions.
Each entry is a plist with :calendar-id, :file, and :label."
  :type '(repeat
          (plist :tag "Calendar"
                 :options ((:calendar-id string)
                           (:label string)
                           (:file file))))
  :group 'google-calendar)

(defun google-calendar--apply-calendars ()
  "Apply `google-calendar-calendars` to `org-gcal-file-alist`."
  (when google-calendar-calendars
    (setq org-gcal-file-alist
          (mapcar (lambda (c)
                    (cons (plist-get c :calendar-id)
                          (plist-get c :file)))
                  google-calendar-calendars))))

(add-hook 'spacemacs-post-user-config-hook
          (lambda ()
            (when google-calendar-calendars
              (google-calendar-define-open-commands)))
          90)

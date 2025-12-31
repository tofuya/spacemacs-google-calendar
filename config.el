(defcustom google-calendar-calendars nil
  "Google Calendar definitions.
Each entry is a plist with :calendar-id and :file."
  :type '(repeat
          (plist :tag "Calendar"
                 :options ((:calendar-id string)
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

(with-eval-after-load 'org-gcal (google-calendar--apply-calendars))
(when google-calendar-calendars (google-calendar-define-open-commands))

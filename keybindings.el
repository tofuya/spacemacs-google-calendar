(defconst google-calendar-leader-root "a g")
(spacemacs/declare-prefix google-calendar-leader-root "google-calendar")

(defconst google-calendar-leader-prefixes
  '((view  . "v")
    (open  . "o")
    (sync  . "s")
    (entry . "e")))

(defun google-calendar-leader-key (&rest keys)
  "Return the combined leader key string.
KEYS are passed as strings or symbols."
  (mapconcat #'identity
             (cons google-calendar-leader-root
                   (mapcar (lambda (k)
                             (if (symbolp k)
                                 (alist-get k google-calendar-leader-prefixes)
                               k))
                           keys))
             " "))

(defun google-calendar-bind (group key fn)
  (let ((full-key (google-calendar-leader-key group key)))
    (spacemacs/set-leader-keys full-key fn)))

(dolist (entry google-calendar-leader-prefixes)
  (spacemacs/declare-prefix
    (concat google-calendar-leader-root " " (cdr entry))
    (capitalize (symbol-name (car entry)))))

(google-calendar-bind 'view "g" #'google-calendar-show-gcal)
(google-calendar-bind 'view "f" #'google-calendar-show-file)
(google-calendar-bind 'view "." #'google-calendar-show-current-buffer)
(google-calendar-bind 'open "f" #'google-calendar-show-file)
(google-calendar-bind 'open "b" #'google-calendar-show-current-buffer)
(google-calendar-bind 'sync "f" #'org-gcal-fetch)
(google-calendar-bind 'sync "r" #'org-gcal-reload-client-id-secret)
(google-calendar-bind 'entry "p" #'org-gcal-post-at-point)
(google-calendar-bind 'entry "d" #'org-gcal-delete-at-point)

(with-eval-after-load 'evil
  (evil-define-key 'normal calfw-calendar-mode-map
    ;; Day / week navigation
    (kbd "h") #'calfw-navi-previous-day-command
    (kbd "l") #'calfw-navi-next-day-command
    (kbd "k") #'calfw-navi-previous-week-command
    (kbd "j") #'calfw-navi-next-week-command

    ;; Week range
    (kbd "^") #'calfw-navi-week-begin-command
    (kbd "$") #'calfw-navi-week-end-command

    ;; Month navigation
    (kbd "K") #'calfw-navi-previous-month-command
    (kbd "J") #'calfw-navi-next-month-command

    ;; View shift (timeline)
    (kbd "H") #'calfw-view-move-previous-command
    (kbd "L") #'calfw-view-move-next-command

    ;; Jump
    (kbd "t") #'calfw-navi-goto-today-command
    (kbd "g") #'calfw-navi-goto-date-command

    ;; View switch (capital = mode change)
    (kbd "M") #'calfw-change-view-month
    (kbd "W") #'calfw-change-view-week
    (kbd "T") #'calfw-change-view-two-weeks
    (kbd "D") #'calfw-change-view-day

    ;; Refresh / quit
    (kbd "r") #'calfw-refresh-calendar-buffer
    (kbd "q") #'bury-buffer

    ;; Item navigation
    (kbd "TAB") #'calfw-navi-next-item-command
    (kbd "RET") #'calfw-open-item-command
    (kbd "SPC") #'calfw-show-details-command))

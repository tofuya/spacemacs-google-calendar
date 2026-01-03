(defconst google-calendar-packages
  '(org-gcal
    (calfw :location (recipe :fetcher github :repo "kiwanami/emacs-calfw" :branch "develop"))))

(defun google-calendar/init-org-gcal ()
  (use-package org-gcal
    :defer t
    :init
    (google-calendar--apply-calendars)
    :config
    (google-calendar--apply-calendars)))

(defun google-calendar/init-calfw-org ()
  (use-package calfw-org
    :defer t
    :after (calfw org)))

(defun google-calendar/init-calfw ()
  (use-package calfw
    :defer t
    :config
    (add-hook 'calfw-calendar-mode-hook
              (lambda () (evil-force-normal-state)))))

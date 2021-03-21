(in-package :stumpwm-settings-gui)

(defun acceptable-type-p (ts)
  (not (and (listp ts)
            (member (car ts) '(member or and)))))

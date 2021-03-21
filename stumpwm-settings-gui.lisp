;;;; stumpwm-settings-gui.lisp

(in-package #:stumpwm-settings-gui)

(define-application-frame stumpwm-settings-inspector ()
  ((type-filter :initform "" :accessor type-filter)
   (active-filter :initform "" :accessor active-filter))
  (:panes (main-display :application
                        :incremental-redisplay t
                        :display-function 'display-main)
          (interactor :interactor))
  (:layouts
   (default
    (labelling (:label "Customize StumpWM Settings"
                :text-style (make-text-style :serif :roman :huge))
      (vertically (:equalize-width t)
        (horizontally ()
          +fill+
          (make-pane 'push-button
                     :label "Save Settings"
                     :activate-callback
                     (lambda (gadget)
                       (declare (ignore gadget))
                       (stumpwm-settings::write-settings-to-file)))
          (make-pane 'push-button
                     :label "Quit"
                     :activate-callback
                     (lambda (gadget)
                       (let ((frame (gadget-client gadget)))
                         (frame-exit frame)))))
        (:fill main-display)
        (1/4 interactor))))))

(defmacro defc (name-and-options arguments &body body)
  `(define-stumpwm-settings-inspector-command ,name-and-options ,arguments ,@body))

(defc (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-presentation-method present
    (config-info (type defconfig::config-info) stream view &key)
  (format stream
          "~&Setting: ~S~%~4TCurrent Value: ~S~%~4TDefault Value: ~S~%~4TType Description: ~S~%"
          (defconfig:config-info-place config-info)
          (symbol-value (defconfig:config-info-place config-info))
          (defconfig:config-info-default-value config-info)
          (or (defconfig:config-info-typespec config-info)
              (defconfig:config-info-valid-values-description config-info))))

(defc (com-customize :name t) ((symbol symbol) (v t))
  (let* ((val v)
         (config-info (handler-case (defconfig::%fsetv-get-config-info-object
                                     symbol (cdr stumpwm-settings::*stumpwm-db*)
                                     'stumpwm-settings::*stumpwm-db*)
                        (error () nil)))
         (validated? (and config-info 
                          (funcall (defconfig:config-info-predicate config-info)
                                   val))))
    (when validated?
      (setf (symbol-value symbol) val))))

(define-presentation-to-command-translator customize-setting
    (defconfig::config-info com-customize stumpwm-settings-inspector
     :priority 1)
    (info)
  (list (defconfig:config-info-place info)
        (let* ((ts (defconfig:config-info-typespec info))
               (acceptable-type (acceptable-type-p ts)))
          (if acceptable-type
              (accept ts)
              (read-from-string (accept 'string))))))

(defc (com-reset-setting :name t) ((symbol symbol))
  (defconfig:reset-computed-place symbol :db stumpwm-settings::*stumpwm-db*))

(define-presentation-to-command-translator reset-setting
    (defconfig::config-info com-reset-setting stumpwm-settings-inspector
     :priority 0)
    (info)
  (list info))

(defc (com-filter-name :name t) ((string string))
  (setf (active-filter *application-frame*) string))

(defc (com-filter-type :name t) ((string string))
  (setf (type-filter *application-frame*) string))

(defun satisfy-filter (frame setting)
  (loop for setting being the hash-value of (cdr stumpwm-settings::*stumpwm-db*)
        if )
  (let ((filter (active-filter frame))
        (dc-symbol (defconfig:config-info-place setting)))
    (cond ((stringp filter)
           (if (string= filter "")
               t
               (let ((val (read-from-string filter)))
                 (eql (defconfig:config-info-place setting) val))))
          ((symbolp filter)
           (eq filter dc-symbol))
          (t nil))))

(defun satisfy-filter (filter setting)
  (if filter 
      (let ((sym-string
              (string-upcase (symbol-name (defconfig:config-info-place setting))))
            (match-against (string-upcase filter)))
        (cl-ppcre:all-matches match-against sym-string))
      t))

(defun satisfy-type-filter (filter setting)
  (if filter
      (let ((match-against (string-upcase filter))
            (ts (string-upcase
                 (format nil "~A" (defconfig:config-info-typespec setting)))))
        (cl-ppcre:all-matches match-against ts))
      t))

(defun display-main (frame pane)
  (with-end-of-line-action (pane :allow)
    (with-end-of-page-action (pane :allow)
      (loop for setting being the hash-value of (cdr stumpwm-settings::*stumpwm-db*)
            with filter = (active-filter frame)
            with tfilter = (type-filter frame)
            when (and (satisfy-filter filter setting)
                      (satisfy-type-filter tfilter setting))
              do (present setting 'defconfig::config-info :stream pane)))))

(defun app-main ()
  (let ((frame (make-application-frame 'stumpwm-settings-inspector)))
    (run-frame-top-level frame)))
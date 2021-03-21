;;;; stumpwm-settings-gui.asd

(asdf:defsystem #:stumpwm-settings-gui
  :description "Describe stumpwm-settings-gui here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:mcclim #:slim #:stumpwm-settings #:defconfig #:cl-ppcre)
  :components ((:file "package")
               (:file "acceptable-types")
               (:file "stumpwm-settings-gui")))

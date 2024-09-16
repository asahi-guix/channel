(define-module (asahi guix systems sway)
  #:use-module (asahi guix systems desktop)
  #:use-module (asahi guix systems edge)
  #:use-module (asahi guix transformations)
  #:use-module (gnu home)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services guix)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (asahi-sway-os))

(define %asahi-desktop-variables
  '(("CLUTTER_BACKEND" . "wayland") ; GTK
    ("QT_QPA_PLATFORM" . "wayland") ; Qt
    ("MOZ_ENABLE_WAYLAND" . "1") ; IceCat, et.al.
    ;; These are normally provided by login managers(?).
    ("XDG_SESSION_TYPE" . "wayland")
    ("XDG_SESSION_DESKTOP" . "sway")
    ("XDG_CURRENT_DESKTOP" . "sway")))

(define %asahi-sway-home-service
  (service guix-home-service-type
           `(("guest" ,(home-environment
                        (services %asahi-desktop-home-services))))))

(define %asahi-sway-packages
  (append (map replace-asahi (list emacs-pgtk dmenu foot sway wofi))
          (remove (lambda (package)
                    (eq? "emacs" (package-name package)))
                  %asahi-desktop-packages)))

(define asahi-sway-os
  (operating-system
    (inherit asahi-edge-os)
    (services (modify-services (cons* %asahi-sway-home-service
                                      %asahi-sddm-service
                                      %asahi-desktop-services)))
    (packages %asahi-sway-packages)))

asahi-sway-os

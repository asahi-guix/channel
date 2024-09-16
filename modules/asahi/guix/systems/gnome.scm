(define-module (asahi guix systems gnome)
  #:use-module (asahi guix systems desktop)
  #:use-module (asahi guix systems edge)
  #:use-module (gnu packages gnome)
  #:use-module (gnu services desktop)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (guix packages)
  #:export (asahi-gnome-os))

(define %asahi-gnome-shell
  (map cadr (modify-inputs (package-propagated-inputs gnome-meta-core-shell)
              (delete "orca" "rygel")))) ;; These packages can't be built.

(define asahi-gnome-os
  (operating-system
    (inherit asahi-edge-os)
    (services (cons* (service gnome-desktop-service-type
                              (gnome-desktop-configuration
                               (shell %asahi-gnome-shell)))
                     %asahi-desktop-home-service
                     %asahi-gdm-service
                     %asahi-desktop-services))
    (packages %asahi-desktop-packages)))

 asahi-gnome-os

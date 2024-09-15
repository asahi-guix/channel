(define-module (asahi guix systems plasma)
  #:use-module (asahi guix systems desktop)
  #:use-module (asahi guix systems edge)
  #:use-module (gnu system)
  #:use-module (gnu packages kde-plasma)
  #:export (asahi-plasma-os))

(define asahi-plasma-os
  (operating-system
    (inherit asahi-edge-os)
    (services (cons* (service plasma-desktop-service-type
                              (plasma-desktop-configuration
                               (plasma-package (replace-asahi plasma))))
                     %asahi-desktop-home-service
                     %asahi-sddm-service
                     %asahi-desktop-services))
    (packages %asahi-desktop-packages)))

asahi-plasma-os

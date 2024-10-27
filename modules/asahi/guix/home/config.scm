(define-module (asahi guix home config)
  #:use-module (asahi guix channels)
  #:use-module (asahi guix home services sound)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:export (asahi-home-environment))

(define services
  (list (service home-channels-service-type asahi-channels)
        (service home-dbus-service-type)
        (service home-pipewire-service-type)))

(define asahi-home-environment
  (home-environment
   (services services)))

asahi-home-environment

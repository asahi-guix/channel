(define-module (asahi guix home services sound)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu home services)
  #:use-module (gnu home services sound))

(define-public asahi-home-pipewire-configuration
  (home-pipewire-configuration
   (pipewire asahi-pipewire)
   (wireplumber asahi-wireplumber)))

(define-public asahi-home-pipewire-service
  (service home-pipewire-service-type
           asahi-home-pipewire-configuration))

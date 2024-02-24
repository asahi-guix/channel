(define-module (asahi guix services sound)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services sound)
  #:export (asahi-alsa-configuration
            asahi-alsa-service))

(define asahi-alsa-configuration
  (alsa-configuration
   (alsa-plugins asahi-alsa-plugins)))

(define-public asahi-alsa-service
  (service alsa-service-type asahi-alsa-configuration))

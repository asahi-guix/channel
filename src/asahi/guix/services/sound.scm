(define-module (asahi guix services sound)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records))

(define-record-type* <alsa-configuration>
  alsa-configuration make-alsa-configuration alsa-configuration?
  (alsa-plugins alsa-configuration-alsa-plugins
                (default asahi-alsa-plugins))
  (pipewire alsa-configuration-pipewire
            (default asahi-pipewire))
  (extra-options alsa-configuration-extra-options
                 (default "")))

(define (alsa-pipewire-config-file config)
  (let ((pipewire (alsa-configuration-pipewire config)))
    #~(string-append #$pipewire "/share/alsa/alsa.conf.d/50-pipewire.conf")))

(define (alsa-pipewire-default-config-file config)
  (let ((pipewire (alsa-configuration-pipewire config)))
    #~(string-append #$pipewire "/share/alsa/alsa.conf.d/99-pipewire-default.conf")))

(define (alsa-etc-service config)
  `(("conf.d/50-pipewire.conf" ,(alsa-pipewire-config-file config))
    ("conf.d/50-pipewire-default.conf" ,(alsa-pipewire-default-config-file config))))

(define-public alsa-service-type
  (service-type
   (name 'alsa)
   (extensions
    (list (service-extension etc-service-type alsa-etc-service)))
   (default-value (alsa-configuration))
   (description "Configure low-level Linux sound support, ALSA.")))

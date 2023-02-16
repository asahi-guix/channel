(define-module (asahi guix services)
  #:use-module (asahi guix udev)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (guix gexp)
  #:export (%channels-service
            %udev-rules-service))

(define %channels-service
  (simple-service 'asahi-channels-file etc-service-type
                  (list `("channels.scm" ,(local-file "channels.scm")))))

(define %udev-rules-service
  (udev-rules-service 'asahi-udev-rules %asahi-udev-rules))

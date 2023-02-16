(define-module (asahi guix services)
  #:use-module (asahi guix udev)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (guix gexp)
  #:export (%channels-service
            %udev-backlight-service
            %udev-kbd-backlight-service))

(define %channels-service
  (simple-service 'asahi-channels-file etc-service-type
                  (list `("channels.scm" ,(local-file "channels.scm")))))

(define %udev-backlight-service
  (udev-rules-service 'backlight %udev-backlight-rule))

(define %udev-kbd-backlight-service
  (udev-rules-service 'kbd-backlight %udev-kbd-backlight-rule))

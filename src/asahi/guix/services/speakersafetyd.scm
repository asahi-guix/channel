(define-module (asahi guix services speakersafetyd)
  #:use-module (asahi guix packages crates-io)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (speakersafetyd-service-type))

(define-record-type* <speakersafetyd-configuration>
  speakersafetyd-configuration
  make-speakersafetyd-configuration
  speakersafetyd-configuration?
  (package speakersafetyd-configuration-package
           (default rust-speakersafetyd-0.1)))

(define (speakersafetyd-shepherd-service config)
  (let ((speakersafetyd (speakersafetyd-configuration-package config)))
    (list (shepherd-service
           (documentation "Asahi Speaker Saftey daemon")
           (provision '(speakersafetyd))
           (start #~(make-forkexec-constructor
                     (list (string-append #$speakersafetyd "/bin/speakersafetyd"))))
           (stop #~(make-kill-destructor))))))

(define speakersafetyd-service-type
  (service-type
   (name 'speakersafetyd)
   (description "Asahi Speaker Saftey daemon")
   (extensions
    (list (service-extension
           shepherd-root-service-type
           speakersafetyd-shepherd-service)))
   (default-value (speakersafetyd-configuration))))

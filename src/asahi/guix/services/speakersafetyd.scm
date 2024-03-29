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
  (blackbox speakersafetyd-configuration-blackbox
            (default "/var/lib/speakersafetyd/blackbox"))
  (max-reduction speakersafetyd-configuration-max-reduction
                 (default 7))
  (package speakersafetyd-configuration-package
           (default rust-speakersafetyd-0.1)))

(define (speakersafetyd-shepherd-service config)
  (let ((blackbox (speakersafetyd-configuration-blackbox config))
        (max-reduction (speakersafetyd-configuration-max-reduction config))
        (package (speakersafetyd-configuration-package config)))
    (list (shepherd-service
           (documentation "Asahi Speaker Saftey daemon")
           (provision '(speakersafetyd))
           (start #~(make-forkexec-constructor
                     (list #$(file-append package "/bin/speakersafetyd")
                           "--blackbox-path" #$blackbox
                           "--max-reduction" (number->string #$max-reduction))))
           (stop #~(make-kill-destructor))))))

(define speakersafetyd-service-type
  (service-type
   (name 'speakersafetyd)
   (description "Asahi Speaker Saftey daemon")
   (extensions
    (list (service-extension
           profile-service-type
           (compose list speakersafetyd-configuration-package))
          (service-extension
           shepherd-root-service-type
           speakersafetyd-shepherd-service)))
   (default-value (speakersafetyd-configuration))))

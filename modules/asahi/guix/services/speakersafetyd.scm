(define-module (asahi guix services speakersafetyd)
  #:use-module (asahi guix packages crates-io)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (speakersafetyd-service-type))

(define-record-type* <speakersafetyd-configuration>
  speakersafetyd-configuration
  make-speakersafetyd-configuration
  speakersafetyd-configuration?
  (config-path speakersafetyd-configuration-config-path
               (default "/usr/share/speakersafetyd"))
  (blackbox speakersafetyd-configuration-blackbox
            (default "/var/lib/speakersafetyd/blackbox"))
  (max-reduction speakersafetyd-configuration-max-reduction
                 (default 7))
  (package speakersafetyd-configuration-package
           (default rust-speakersafetyd-1)))

(define (speakersafetyd-shepherd-service config)
  (let ((blackbox (speakersafetyd-configuration-blackbox config))
        (config-path (speakersafetyd-configuration-config-path config))
        (max-reduction (speakersafetyd-configuration-max-reduction config))
        (package (speakersafetyd-configuration-package config)))
    (list (shepherd-service
           (documentation "Speaker saftey daemon")
           (provision '(speakersafetyd))
           (requirement '(udev))
           (start #~(make-forkexec-constructor
                     (list #$(file-append package "/bin/speakersafetyd")
                           "--config-path" #$(file-append package config-path)
                           "--blackbox-path" #$blackbox
                           "--max-reduction" (number->string #$max-reduction))))
           (stop #~(make-kill-destructor))))))

(define speakersafetyd-service-type
  (service-type
   (name 'speakersafetyd)
   (description "Speaker saftey daemon")
   (extensions
    (list (service-extension
           profile-service-type
           (compose list speakersafetyd-configuration-package))
          (service-extension
           shepherd-root-service-type
           speakersafetyd-shepherd-service)
          (service-extension
           udev-service-type
           (compose list speakersafetyd-configuration-package))))
   (default-value (speakersafetyd-configuration))))

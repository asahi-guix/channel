(define-module (asahi guix services speakersafetyd)
  #:use-module (asahi guix packages crates-io)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:export (speakersafetyd-service-type))

(define (speakersafetyd-shepherd-service config)
  (list (shepherd-service
         (documentation "Run the speaker saftey daemon.")
         (provision '(speakersafetyd))
         (start #~(make-forkexec-constructor
                   (list #$(file-append rust-speakersafetyd-0.1 "/bin/speakersafetyd"))
                   #:pid-file "/var/run/speakersafetyd.pid"))
         (stop #~(make-kill-destructor)))))

(define speakersafetyd-service-type
  (service-type
   (name 'speakersafetyd)
   (description "Run the speaker saftey daemon.")
   (extensions
    (list (service-extension
           shepherd-root-service-type
           speakersafetyd-shepherd-service)))
   (default-value #f)))

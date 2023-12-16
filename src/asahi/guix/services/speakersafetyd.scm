(define-module (asahi guix services speakersafetyd)
  #:use-module (asahi guix packages crates-io)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp))

(define speakersafetyd-service
  (shepherd-service
   (documentation "Run the speaker saftey daemon.")
   (provision '(speakersafetyd))
   (start #~(make-forkexec-constructor
             (list #$(file-append rust-speakersafetyd-0.1 "/bin/speakersafetyd"))
             #:pid-file "/var/run/speakersafetyd.pid"))
   (stop #~(make-kill-destructor))))

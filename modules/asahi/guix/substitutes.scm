(define-module (asahi guix substitutes)
  #:use-module (asahi guix channels)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (ice-9 optargs)
  #:export (append-substitutes))

(define-public %authorized-keys
  (list (local-file "files/authorized-keys/substitutes.asahi-guix.org.pub")))

(define-public %substitute-urls
  (list "https://substitutes.asahi-guix.org"))

(define* (append-substitutes
          config
          #:key
          (authorized-keys %authorized-keys)
          (substitute-urls %substitute-urls))
  (guix-configuration
   (inherit config)
   (authorized-keys
    (append
     (guix-configuration-authorized-keys config)
     authorized-keys))
   ;; Using guix-for-channels causes Cuirass to fail with the following error:
   ;; #<&inferior-exception arguments: (git-error #<inferior-object
   ;; #<<git-error> code: -3 message: "could not find repository at
   ;; (guix (guix-for-channels (list asahi-channel guix-channel)))
   (substitute-urls
    (append
     (guix-configuration-substitute-urls config)
     substitute-urls))))

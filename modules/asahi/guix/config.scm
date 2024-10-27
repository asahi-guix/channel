(define-module (asahi guix config)
  #:use-module (guix gexp)
  #:export (%asahi-substitute-keys
            %asahi-substitute-urls
            %asahi-version))

(define %asahi-substitute-keys
  (list (local-file "files/authorized-keys/substitutes.asahi-guix.org.pub")))

(define %asahi-substitute-urls
  (list "https://substitutes.asahi-guix.org"))

(define %asahi-version "0.0.1")

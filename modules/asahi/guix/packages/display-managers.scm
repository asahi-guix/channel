(define-module (asahi guix packages display-managers)
  #:use-module (asahi guix packages gl)
  #:use-module (gnu packages display-managers)
  #:use-module (guix packages))

(define-public asahi-slim
  ((package-input-rewriting/spec
    `(("mesa" . ,(const asahi-mesa))))
   (package/inherit slim
     (name "asahi-slim"))))

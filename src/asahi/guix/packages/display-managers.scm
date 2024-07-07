(define-module (asahi guix packages display-managers)
  #:use-module (asahi guix packages gl)
  #:use-module (gnu packages display-managers)
  #:use-module (guix packages))

(define replace-mesa
  (package-input-rewriting/spec
   `(("mesa" . ,(const asahi-mesa)))))

(define-public asahi-slim
  (package
    (inherit (replace-mesa slim))
    (name "asahi-slim")))

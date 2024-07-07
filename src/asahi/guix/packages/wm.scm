(define-module (asahi guix packages wm)
  #:use-module (asahi guix packages gl)
  #:use-module (gnu packages wm)
  #:use-module (guix packages))

(define replace-mesa
  (package-input-rewriting/spec
   `(("mesa" . ,(const asahi-mesa)))))

(define-public asahi-sway
  (package
    (inherit (replace-mesa sway))
    (name "asahi-sway")))

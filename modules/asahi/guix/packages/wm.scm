(define-module (asahi guix packages wm)
  #:use-module (asahi guix packages gl)
  #:use-module (gnu packages wm)
  #:use-module (guix packages))

(define-public asahi-sway
  ((package-input-rewriting/spec
    `(("mesa" . ,(const asahi-mesa))))
   (package/inherit sway
     (name "asahi-sway"))))

(define-module (asahi guix packages xorg)
  #:use-module (asahi guix packages gl)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages))

(define-public asahi-xorg-server
  ((package-input-rewriting/spec
    `(("mesa" . ,(const asahi-mesa))))
   (package/inherit xorg-server
     (name "asahi-xorg-server"))))

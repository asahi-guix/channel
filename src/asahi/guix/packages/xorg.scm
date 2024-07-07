(define-module (asahi guix packages xorg)
  #:use-module (asahi guix packages gl)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages))

(define replace-mesa
  (package-input-rewriting/spec
   `(("mesa" . ,(const asahi-mesa)))))

(define-public asahi-xorg-server
  (package
   (inherit (replace-mesa xorg-server))
    (name "asahi-xorg-server")))

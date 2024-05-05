(define-module (asahi guix packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages gl)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages))

(define-public asahi-xorg-server
  (package/inherit xorg-server
    (name "asahi-xorg-server")
    (inputs
     (modify-inputs (package-inputs xorg-server)
       (replace "mesa" asahi-mesa)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs xorg-server)
       (replace "mesa" asahi-mesa)))))

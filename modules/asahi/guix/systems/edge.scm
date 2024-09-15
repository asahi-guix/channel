(define-module (asahi guix systems edge)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix systems base)
  #:use-module (gnu system)
  #:export (asahi-edge-os))

(define asahi-edge-os
  (operating-system
    (inherit asahi-base-os)
    (kernel asahi-linux-edge)
    (initrd-modules asahi-initrd-modules-edge)))

asahi-edge-os

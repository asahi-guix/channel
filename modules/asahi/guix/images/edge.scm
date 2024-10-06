(define-module (asahi guix images edge)
  #:use-module (asahi guix systems edge)
  #:use-module (gnu system images asahi)
  #:export (asahi-edge-os-image))

(define asahi-edge-os-image
  (make-asahi-image 'asahi-guix-edge asahi-edge-os))

asahi-edge-os-image

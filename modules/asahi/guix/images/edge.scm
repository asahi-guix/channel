(define-module (asahi guix images edge)
  #:use-module (asahi guix systems edge)
  #:use-module (gnu system images asahi)
  #:export (asahi-guix-edge-image))

(define asahi-guix-edge-image
  (make-asahi-image 'asahi-guix-edge asahi-edge-os))

asahi-guix-edge-image

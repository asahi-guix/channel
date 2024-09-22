(define-module (asahi guix images edge)
  #:use-module (asahi guix systems edge)
  #:use-module (gnu system images asahi)
  #:export (asahi-edge-image))

(define asahi-edge-image
  (make-asahi-image 'asahi-edge-image asahi-edge-os))

asahi-edge-image

(define-module (asahi guix images edge)
  #:use-module (asahi guix systems edge)
  #:use-module (gnu system images asahi installer)
  #:export (asahi-edge-os-image))

(define asahi-edge-os-image
  (make-installer-image 'asahi-guix-edge asahi-edge-os))

asahi-edge-os-image

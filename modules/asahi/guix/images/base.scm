(define-module (asahi guix images base)
  #:use-module (asahi guix systems base)
  #:use-module (gnu system images asahi)
  #:export (asahi-base-image))

(define asahi-base-image
  (make-asahi-image 'asahi-base-image asahi-base-os))

asahi-base-image

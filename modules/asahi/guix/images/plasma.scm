(define-module (asahi guix images plasma)
  #:use-module (asahi guix systems plasma)
  #:use-module (gnu system images asahi)
  #:export (asahi-plasma-image))

(define asahi-plasma-image
  (make-asahi-image 'asahi-plasma-image asahi-plasma-os))

asahi-plasma-image

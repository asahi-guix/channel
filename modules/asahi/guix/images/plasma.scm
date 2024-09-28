(define-module (asahi guix images plasma)
  #:use-module (asahi guix systems plasma)
  #:use-module (gnu system images asahi)
  #:export (asahi-guix-plasma-image))

(define asahi-guix-plasma-image
  (make-asahi-image 'asahi-guix-plasma asahi-plasma-os))

asahi-guix-plasma-image

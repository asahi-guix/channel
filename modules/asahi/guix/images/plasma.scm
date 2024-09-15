(define-module (asahi guix images plasma)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix systems plasma)
  #:export (asahi-plasma-image))

(define asahi-plasma-image
  (make-image asahi-plasma-os 'asahi-plasma-image))

asahi-plasma-image

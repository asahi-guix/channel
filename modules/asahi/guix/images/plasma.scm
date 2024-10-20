(define-module (asahi guix images plasma)
  #:use-module (asahi guix systems plasma)
  #:use-module (gnu system images asahi installer)
  #:export (asahi-plasma-os-image))

(define asahi-plasma-os-image
  (make-installer-image 'asahi-plasma-image asahi-plasma-os))

asahi-plasma-os-image

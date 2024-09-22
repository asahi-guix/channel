(define-module (asahi guix images gnome)
  #:use-module (asahi guix systems gnome)
  #:use-module (gnu system images asahi)
  #:export (asahi-gnome-image))

(define asahi-gnome-image
  (make-asahi-image 'asahi-gnome-image asahi-gnome-os))

asahi-gnome-image

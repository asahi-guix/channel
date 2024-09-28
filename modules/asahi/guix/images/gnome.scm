(define-module (asahi guix images gnome)
  #:use-module (asahi guix systems gnome)
  #:use-module (gnu system images asahi)
  #:export (asahi-guix-gnome-image))

(define asahi-guix-gnome-image
  (make-asahi-image 'asahi-guix-gnome asahi-gnome-os))

asahi-guix-gnome-image

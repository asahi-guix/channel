(define-module (asahi guix images gnome)
  #:use-module (asahi guix systems gnome)
  #:use-module (gnu system images asahi)
  #:export (asahi-gnome-os-image))

(define asahi-gnome-os-image
  (make-asahi-image 'asahi-guix-gnome asahi-gnome-os))

asahi-gnome-os-image

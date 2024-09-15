(define-module (asahi guix images gnome)
  #:use-module (asahi guix images base)
  #:use-module (asahi guix systems gnome)
  #:export (asahi-gnome-image))

(define asahi-gnome-image
  (make-image asahi-gnome-os 'asahi-gnome-image))

asahi-gnome-image

(define-module (asahi guix images gnome)
  #:use-module (asahi guix systems gnome)
  #:use-module (gnu system images asahi installer)
  #:export (asahi-gnome-os-image))

(define asahi-gnome-os-image
  (make-installer-image 'asahi-gnome-image asahi-gnome-os))

asahi-gnome-os-image

(define-module (asahi guix images installer)
  #:use-module (asahi guix systems install)
  #:use-module (gnu system images asahi installer)
  #:export (asahi-installer-os-image))

(define asahi-installer-os-image
  (make-installer-image 'asahi-installer-image asahi-installation-os))

asahi-installer-os-image

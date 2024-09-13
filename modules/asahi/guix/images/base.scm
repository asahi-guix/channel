(define-module (asahi guix images base)
  #:use-module (asahi guix systems base)
  #:use-module (gnu image)
  #:use-module (gnu system image)
  #:use-module (guix platforms arm))

(define-public asahi-image-type
  (image-type
   (name 'asahi-base-raw)
   (constructor (lambda (os)
                  (image
                   (inherit efi-disk-image)
                   (operating-system os)
                   (platform aarch64-linux))))))

(define-public asahi-base-image
  (image
   (inherit
    (os+platform->image
     asahi-base-os
     aarch64-linux
     #:type asahi-image-type))
   (name 'asahi-base-image)))

asahi-base-image

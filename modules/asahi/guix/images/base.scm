(define-module (asahi guix images base)
  #:use-module (asahi guix images installer)
  #:use-module (asahi guix systems base)
  #:use-module (gnu image)
  #:use-module (gnu system image)
  #:use-module (guix platforms arm)
  #:export (asahi-base-image
            asahi-image-type
            make-image))

(define (constructor os)
  (image
   (inherit efi-disk-image)
   (operating-system os)
   (platform aarch64-linux)))

(define asahi-image-type
  (image-type
   (name 'asahi-base-raw)
   (constructor constructor)))

(define (make-image os name)
  (image
   (inherit
    (os+platform->image
     os aarch64-linux
     ;; TODO: Which image?
     #:type asahi-installer-image-type))
   (name name)))

(define asahi-base-image
  (make-image asahi-base-os 'asahi-base-image))

asahi-base-image

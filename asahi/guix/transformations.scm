(define-module (asahi guix transformations)
  #:use-module ((asahi guix packages))
  #:use-module ((asahi guix packages jemalloc) #:prefix next:)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages jemalloc)
  #:use-module (guix packages)
  #:export (replace-jemalloc replace-mesa))

(define replace-jemalloc
  (package-input-rewriting `((,jemalloc . ,next:jemalloc))))

(define replace-mesa
  (package-input-rewriting `((,mesa . ,mesa-asahi-edge))))

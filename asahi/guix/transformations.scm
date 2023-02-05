(define-module (asahi guix transformations)
  #:use-module ((asahi guix packages jemalloc) #:prefix next:)
  #:use-module (gnu packages jemalloc)
  #:use-module (guix packages)
  #:export (replace-jemalloc))

(define replace-jemalloc
  (package-input-rewriting `((,jemalloc . ,next:jemalloc))))

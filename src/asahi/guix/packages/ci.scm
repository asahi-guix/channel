(define-module (asahi guix packages ci)
  #:use-module ((gnu packages ci) #:prefix ci:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages))

(define-public cuirass-disable-jit
  (package
    (inherit (package-with-patches
              ci:cuirass
              (list (local-file "../patches/cuirass-disable-jit.patch"))))
    (name "cuirass-disable-jit")))

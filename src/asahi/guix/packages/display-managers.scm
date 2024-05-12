(define-module (asahi guix packages display-managers)
  #:use-module (asahi guix packages gl)
  #:use-module (gnu packages display-managers)
  #:use-module (guix packages))

(define-public asahi-slim
  (package/inherit slim
    (name "asahi-slim")
    (inputs
     (modify-inputs (package-inputs slim)
       (replace "freeglut" asahi-freeglut)))))

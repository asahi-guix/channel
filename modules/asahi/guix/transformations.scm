(define-module (asahi guix transformations)
  #:use-module (asahi guix packages gl)
  #:use-module (guix packages))

(define-public replace-asahi
  (package-input-rewriting/spec
   `(("mesa" . ,(const asahi-mesa))
     ("mesa-utils" . ,(const asahi-mesa-utils)))))

(define-public replace-mesa
  (package-input-rewriting/spec
   `(("mesa" . ,(const asahi-mesa))
     ("mesa-utils" . ,(const asahi-mesa-utils)))))

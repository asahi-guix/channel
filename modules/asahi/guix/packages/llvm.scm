(define-module (asahi guix packages llvm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages llvm)
  #:use-module (guix packages))

(define-public asahi-libclc
  ((package-input-rewriting/spec
    `(("clang" . ,(const clang-18))
      ("llvm" . ,(const llvm-18))))
   (package/inherit libclc
     (name "asahi-libclc")
     (version (package-version llvm-18)))))

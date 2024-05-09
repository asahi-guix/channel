(define-module (asahi guix packages llvm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages llvm)
  #:use-module (guix packages))

(define-public asahi-libclc
  (package/inherit libclc
    (name "asahi-libclc")
    (version (package-version llvm-18))
    (native-inputs
     (modify-inputs (package-native-inputs libclc)
       (replace "clang" clang-18)
       (replace "llvm" llvm-18)))))

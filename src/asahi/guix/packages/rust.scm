(define-module (asahi guix packages rust)
  #:use-module ((gnu packages rust) #:prefix rust:)
  #:use-module (gnu packages llvm)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define rust-bootstrapped-package
  (@@ (gnu packages rust) rust-bootstrapped-package))

(define-public rust-1.76
  (let ((base-rust (rust-bootstrapped-package rust:rust-1.75 "1.76.0"
                    "08f06shp6l72qrv5fwg1is7yzr6kwj8av0l9h5k243bz781zyp4y")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches '())))
      (inputs (modify-inputs (package-inputs base-rust)
                             (replace "llvm" llvm-16))))))

(define-public rust rust-1.76)

(define-public rust-src
  (package
    (inherit rust)
    (name "rust-src")
    (build-system copy-build-system)
    (native-inputs '())
    (inputs '())
    (native-search-paths '())
    (outputs '("out"))
    (arguments
     `(#:install-plan
       '(("library" "lib/rustlib/src/rust/library")
         ("src" "lib/rustlib/src/rust/src"))))
    (synopsis "Source code for the Rust standard library")
    (description "This package provide source code for the Rust standard library, only
use by rust-analyzer, make rust-analyzer out of the box.")))

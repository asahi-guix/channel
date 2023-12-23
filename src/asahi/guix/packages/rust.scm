(define-module (asahi guix packages rust)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public rust
  (@@ (gnu packages rust) rust-1.71))

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

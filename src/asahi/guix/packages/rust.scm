(define-module (asahi guix packages rust)
  #:use-module ((gnu packages rust) #:prefix rust:)
  #:use-module (guix build-system copy)
  #:use-module (guix packages))

(define-public rust rust:rust-1.78)

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

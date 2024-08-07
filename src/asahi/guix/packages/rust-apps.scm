(define-module (asahi guix packages rust-apps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public rust-bindgen-0.69
  (package
    (name "rust-bindgen")
    (version "0.69.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18194611hn3k1dkxlha7a52sr8vmfhl9blc54xhj08cahd8wh3d0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-annotate-snippets" ,rust-annotate-snippets-0.9)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-cexpr" ,rust-cexpr-0.6)
                       ("rust-clang-sys" ,rust-clang-sys-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-lazycell" ,rust-lazycell-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
                       ("rust-prettyplease" ,rust-prettyplease-0.2)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-syn" ,rust-syn-2)
                       ("rust-which" ,rust-which-4))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

(define-public rust-bindgen-cli-0.69
  (package
    (name "rust-bindgen-cli")
    (version "0.69.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00dfny07m4xfnqbfn7yr7cqwilj6935lbyg5d39yxjfj8jglfcax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-complete" ,rust-clap-complete-4)
                       ("rust-env-logger" ,rust-env-logger-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-shlex" ,rust-shlex-1))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))

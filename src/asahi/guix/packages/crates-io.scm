(define-module (asahi guix packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix packages rust)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public rust-apple-nvram-0.2
  (package
    (name "rust-apple-nvram")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "apple-nvram" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0v6cv4v0m1z46vaj53vb3z9mnkcjc364ql09zyiyzlcqblvhyljx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-adler32" ,rust-adler32-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-nix" ,rust-nix-0.26))))
    (home-page "https://github.com/WhatAmISupposedToPutHere/asahi-nvram")
    (synopsis "Library to parse and write apple-formatted nvram entries")
    (description
     "This package provides a library to parse and write apple-formatted nvram entries")
    (license license:expat)))

(define-public rust-asahi-nvram-0.2
  (package
    (name "rust-asahi-nvram")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asahi-nvram" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04qp73w97792flzkv9b3qb549iwa9mam837b0l8zh04lcnbcgci6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-apple-nvram" ,rust-apple-nvram-0.2)
                       ("rust-clap" ,rust-clap-3))))
    (home-page "https://github.com/WhatAmISupposedToPutHere/asahi-nvram")
    (synopsis "Tool to read and write nvram variables on ARM Macs")
    (description
     "This package provides a tool to read and write nvram variables on ARM Macs")
    (license license:expat)))

(define-public rust-prettyplease-0.2
  (package
    (name "rust-prettyplease")
    (version "0.2.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prettyplease" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dfbq98rkq86l9g8w1l81bdvrz4spcfl48929n0pyz79clhzc754"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/prettyplease")
    (synopsis "Minimal `syn` syntax tree pretty-printer")
    (description
     "This package provides a minimal `syn` syntax tree pretty-printer")
    (license (list license:expat license:asl2.0))))

(define-public rust-yansi-term-0.1
  (package
    (name "rust-yansi-term")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yansi-term" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w8vjlvxba6yvidqdvxddx3crl6z66h39qxj8xi6aqayw2nk0p7y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/botika/yansi-term")
    (synopsis "Library for ANSI terminal colours and styles (bold, underline)")
    (description
     "Library for ANSI terminal colours and styles (bold, underline)")
    (license license:expat)))

(define-public rust-annotate-snippets-0.9
  (package
    (name "rust-annotate-snippets")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "annotate-snippets" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07p8r6jzb7nqydq0kr5pllckqcdxlyld2g275v425axnzffpxbyc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-yansi-term" ,rust-yansi-term-0.1))))
    (home-page "https://github.com/rust-lang/annotate-snippets-rs")
    (synopsis "Library for building code annotations")
    (description "Library for building code annotations")
    (license (list license:asl2.0 license:expat))))

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

(define-public rust-biquad-0.4
  (package
    (name "rust-biquad")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "biquad" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gpc13lag439nmq077wfwz055qbjaxbpk7znvnbddbg3wgsj81c2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libm" ,rust-libm-0.1))))
    (home-page "https://github.com/korken89/biquad-rs")
    (synopsis
     "A library for digital second order IIR filtrers, also known as biquads.")
    (description
     "This package provides a library for digital second order IIR filtrers, also
known as biquads.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-worker-0.1
  (package
    (name "rust-lv2-worker")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-worker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14crsrnjyarra9ipma6lhaj4gpfadvippzr134nkn0z3y30ip4fj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's work offloading library")
    (description "rust-lv2's work offloading library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-urid-2
  (package
    (name "rust-lv2-urid")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-urid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2fcb0nyn54ml6azkbhnnxghy898x1q5vs5qgdznrhy9m20624c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's URID handling library")
    (description "rust-lv2's URID handling library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-time-0.1
  (package
    (name "rust-lv2-time")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wznk17vvn5dph6r47vjwmf7g98pb6ij2fdhizdk95sf2qvkf82c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's wrapper of LV2's time types")
    (description "rust-lv2's wrapper of LV2's time types")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-state-2
  (package
    (name "rust-lv2-state")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nm0fc7cb4rkmfsvvr4xbac4qf0j7wl2gws3qrcflx057i2lpsb5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's state handling library")
    (description "rust-lv2's state handling library")
    (license (list license:expat license:asl2.0))))

(define-public rust-wmidi-3
  (package
    (name "rust-wmidi")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wmidi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kxnbs18nmpzm2hfwaaa5h2s77cmk5w53srzxqmrqlkdpdcrjafa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RustAudio/wmidi")
    (synopsis "Midi parsing library.")
    (description "Midi parsing library.")
    (license license:expat)))

(define-public rust-lv2-midi-1
  (package
    (name "rust-lv2-midi")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-midi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x0glbrfri1glgcrmvc6i1jfv6azhpqvp4ibk5cihsq3s2yfc8xd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1)
                       ("rust-wmidi" ,rust-wmidi-3))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's MIDI processing library")
    (description "rust-lv2's MIDI processing library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-units-0.1
  (package
    (name "rust-lv2-units")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-units" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fdamp3hxdr36hqi1j6y01rz1x17if1ibzr7rr4nrabidw74gf82"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's wrapper of LV2's unit types")
    (description "rust-lv2's wrapper of LV2's unit types")
    (license (list license:expat license:asl2.0))))

(define-public rust-urid-derive-0.1
  (package
    (name "rust-urid-derive")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urid-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i1nf0sgq4ai051h17s9msaavl3jfzdmdlsy8455pr88y0pfx7l1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Procedural macros for urid")
    (description "Procedural macros for urid")
    (license (list license:expat license:asl2.0))))

(define-public rust-urid-0.1
  (package
    (name "rust-urid")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "195672gs136vczn1r4hkjg5vfa7vdzr26bzv6lwhk0z7cvbvaa38"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-urid-derive" ,rust-urid-derive-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Library for idiomatic URID support")
    (description "Library for idiomatic URID support")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-sys-2
  (package
    (name "rust-lv2-sys")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c4f59mrjyy0z0wf033wp648df0sc6zirrcd6kndqj9nvvkzkl4x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's C header bindings")
    (description "rust-lv2's C header bindings")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-core-derive-2
  (package
    (name "rust-lv2-core-derive")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-core-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12w3l41jzargrcywz13hbmaazfw4ix2sljl3601h6jfbdrw8zybv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Procedural macros for lv2-core")
    (description "Procedural macros for lv2-core")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-core-3
  (package
    (name "rust-lv2-core")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pj9l15zwqwj2h83f3xfpwxsj70vvhkw52gyzkljafvrbx1h00fm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-core-derive" ,rust-lv2-core-derive-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's core library")
    (description "rust-lv2's core library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-atom-2
  (package
    (name "rust-lv2-atom")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-atom" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wd9rgsn8sag8wyhjccmnn82gx4w1yyiav52nyvk579l21xlw6wm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-lv2-units" ,rust-lv2-units-0.1)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's Atom handling library")
    (description "rust-lv2's Atom handling library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-0.6
  (package
    (name "rust-lv2")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xh4hjfh2w5rhzbk0g9845k25f6fxrv7xqpkr09p0x57b200qc41"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-midi" ,rust-lv2-midi-1)
                       ("rust-lv2-state" ,rust-lv2-state-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-lv2-time" ,rust-lv2-time-0.1)
                       ("rust-lv2-units" ,rust-lv2-units-0.1)
                       ("rust-lv2-urid" ,rust-lv2-urid-2)
                       ("rust-lv2-worker" ,rust-lv2-worker-0.1)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "A safe, fast, and ergonomic framework to create LV2 plugins")
    (description
     "This package provides a safe, fast, and ergonomic framework to create LV2
plugins")
    (license (list license:expat license:asl2.0))))

(define-public rust-bankstown
  (package
    (name "rust-bankstown")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chadmed/bankstown")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1a4lickqwkd8kdnsfxwz8rnv3kfp7xjlj3qd1f63vgd6iw8mff11"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-biquad" ,rust-biquad-0.4)
                       ("rust-lv2" ,rust-lv2-0.6))))
    (home-page "https://github.com/chadmed/bankstown")
    (synopsis "Barebones bass enhancer")
    (description "Halfway-decent three-stage psychoacoustic bass approximation.")
    (license license:expat)))

(define-public rust-simple-logger-1
  (package
    (name "rust-simple-logger")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "simple_logger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pkvkp0v3w9kwqjhx5npb2jbyj9kfbb8y2w92s5cphsxldc05dj5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atty" ,rust-atty-0.2)
                       ("rust-colored" ,rust-colored-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/borntyping/rust-simple_logger")
    (synopsis
     "A logger that prints all messages with a readable output format")
    (description
     "This package provides a logger that prints all messages with a readable output
format")
    (license license:expat)))

(define-public rust-signal-hook-0.3
  (package
    (name "rust-signal-hook")
    (version "0.3.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "signal-hook" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0098nsah04spqf3n8niirmfym4wsdgjl57c78kmzijlq8xymh8c6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-signal-hook-registry" ,rust-signal-hook-registry-1))))
    (home-page "https://github.com/vorner/signal-hook")
    (synopsis "Unix signal handling")
    (description "Unix signal handling")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.153")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gg7m1ils5dms5miq9fyllrcp0jxnbpgkx71chd2i0lafa8qy6cw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zerocopy-derive-0.7
  (package
    (name "rust-zerocopy-derive")
    (version "0.7.32")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zerocopy-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19nj11md42aijyqnfx8pa647fjzhz537xyc624rajwwfrn6b3qcw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/google/zerocopy")
    (synopsis "Custom derive for traits from the zerocopy crate")
    (description "Custom derive for traits from the zerocopy crate")
    (license (list license:bsd-2 license:asl2.0 license:expat))))

(define-public rust-zerocopy-0.7
  (package
    (name "rust-zerocopy")
    (version "0.7.32")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zerocopy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ghnfxw69kx5d1aqfd5fsfrra9dgpz17yqx84nd4ryjk3sbd7m3l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-zerocopy-derive" ,rust-zerocopy-derive-0.7))))
    (home-page "https://github.com/google/zerocopy")
    (synopsis "Utilities for zero-copy parsing and serialization")
    (description "Utilities for zero-copy parsing and serialization")
    (license (list license:bsd-2 license:asl2.0 license:expat))))

(define-public rust-ahash-0.8
  (package
    (name "rust-ahash")
    (version "0.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ahash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zxa0dx8i2kkgd9sk1i8276dg5s8ga6n0yw8gb3n3w9x5l855ka2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atomic-polyfill" ,rust-atomic-polyfill-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-const-random" ,rust-const-random-0.1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-version-check" ,rust-version-check-0.9)
                       ("rust-zerocopy" ,rust-zerocopy-0.7))))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
     "A non-cryptographic hash function using AES-NI for high performance")
    (description
     "This package provides a non-cryptographic hash function using AES-NI for high
performance")
    (license (list license:expat license:asl2.0))))

(define-public rust-hashbrown-0.14
  (package
    (name "rust-hashbrown")
    (version "0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hashbrown" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "012nywlg0lj9kwanh69my5x67vjlfmzfi9a0rq4qvis2j8fil3r9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-allocator-api2" ,rust-allocator-api2-0.2)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-equivalent" ,rust-equivalent-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "A Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's @code{SwissTable} hash map")
    (license (list license:expat license:asl2.0))))

(define-public rust-indexmap-2
  (package
    (name "rust-indexmap")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indexmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xy1wcad2da199f6y0mwmx9scw7glgs1n2g4m8nfln7hcf8g6g13"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-equivalent" ,rust-equivalent-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-rayon" ,rust-rustc-rayon-0.5)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis "A hash table with consistent order and fast iteration.")
    (description
     "This package provides a hash table with consistent order and fast iteration.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-configparser-3
  (package
    (name "rust-configparser")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "configparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1202hawv78c3w2j8s44jjcflnpvkwgv3l1irafl7f0smivdd7ijf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-indexmap" ,rust-indexmap-2))))
    (home-page "https://github.com/QEDK/configparser-rs")
    (synopsis
     "A simple configuration parsing utility with no dependencies that allows you to parse INI and ini-style syntax. You can use this to write Rust programs which can be customized by end users easily.")
    (description
     "This package provides a simple configuration parsing utility with no
dependencies that allows you to parse INI and ini-style syntax.  You can use
this to write Rust programs which can be customized by end users easily.")
    (license (list license:expat license:lgpl3+))))

(define-public rust-clap-verbosity-flag-2
  (package
    (name "rust-clap-verbosity-flag")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap-verbosity-flag" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y54kb14nr2vddl5j5h1s4217hbnxfxh7ln8j7lw5r2qvp0216xv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/clap-rs/clap-verbosity-flag")
    (synopsis "Easily add a `--verbose` flag to CLIs using Clap")
    (description "Easily add a `--verbose` flag to CLIs using Clap")
    (license (list license:expat license:asl2.0))))

(define-public rust-pure-rust-locales-0.7
  (package
    (name "rust-pure-rust-locales")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pure-rust-locales" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cl46srhxzj0jlvfp73l8l9qw54qwa04zywaxdf73hidwqlsh0pd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/cecton/pure-rust-locales")
    (synopsis
     "Pure Rust locales imported directly from the GNU C Library. `LC_COLLATE` and `LC_CTYPE` are not yet supported.")
    (description
     "Pure Rust locales imported directly from the GNU C Library. `LC_COLLATE` and
`LC_CTYPE` are not yet supported.")
    (license (list license:expat license:asl2.0))))

(define-public rust-android-tzdata-0.1
  (package
    (name "rust-android-tzdata")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "android-tzdata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RumovZ/android-tzdata")
    (synopsis "Parser for the Android-specific tzdata file")
    (description "Parser for the Android-specific tzdata file")
    (license (list license:expat license:asl2.0))))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.34")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chrono" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12zk0ja924f55va2fs0qj34xaygq46fy92blmc7qkmcj9dj1bh2v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-tzdata" ,rust-android-tzdata-0.1)
                       ("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-iana-time-zone" ,rust-iana-time-zone-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pure-rust-locales" ,rust-pure-rust-locales-0.7)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-windows-targets" ,rust-windows-targets-0.48))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-alsa-0.8
  (package
    (name "rust-alsa")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "alsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02pzlq2q8ml28ikvkvm77bwdqmi22d6ak1qvrc0cr6yjb9adwd6f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.26))))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin but safe wrappers for ALSA (Linux sound API)")
    (description "Thin but safe wrappers for ALSA (Linux sound API)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-speakersafetyd-0.1
  (package
    (name "rust-speakersafetyd")
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "speakersafetyd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dsy2gbrqiv6ky9mz09z95i111yqi16dn13vi9m8hc7gwv3bk0pj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-alsa" ,rust-alsa-0.8)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-verbosity-flag" ,rust-clap-verbosity-flag-2)
                       ("rust-configparser" ,rust-configparser-3)
                       ("rust-json" ,rust-json-0.12)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-simple-logger" ,rust-simple-logger-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/main.rs"
               (("share/speakersafetyd") "share")
               (("/usr/local") (assoc-ref outputs "out")))))
         (add-after 'install 'install-conf
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out") "/share")))
               (copy-recursively "conf" target)))))))
    (inputs
     (list asahi-alsa-lib))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/AsahiLinux/speakersafetyd/")
    (synopsis "Speaker protection daemon for embedded Linux systems")
    (description "Speakersafetyd is a userspace daemon written in Rust that implements
an analogue of the Texas Instruments Smart Amp speaker protection
model.")
    (license license:expat)))

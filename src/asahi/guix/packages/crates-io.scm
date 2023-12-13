(define-module (asahi guix packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages rust)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public rust-bindgen-0.69
  (package
    (name "rust-bindgen")
    (version "0.69.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hkrccfri0223b2r5cvacy83ld6s76n2m68518bsfilrhk1ypz4z"))))
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
    (version "0.69.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00dm53rclzqr326fx1jk0q1smf83spzgp5wq55w9xa8hj7h5wml8"))))
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

(define-public rust-bankstown
  (package
    (name "rust-bankstown")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chadmed/bankstown")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "084p4rf06m4ywiai1lj079p9fa3dkx555vmp5q9dzrmf6fxbsffr"))))
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
    (version "0.2.151")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x28f0zgp4zcwr891p8n9ag9w371sbib30vp4y6hi2052frplb9h"))))
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
    (version "0.7.30")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zerocopy-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fpnplh9k89mj372wnv5s5c5jh0qn9f42wqvznz8za1mhbv2p4dy"))))
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
    (version "0.7.30")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zerocopy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ixmmjkhlzc2hb3szvwcfl8ipsz4nfv0ihsyccqiz3siam2clv9h"))))
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
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ahash" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0yn9i8nc6mmv28ig9w3dga571q09vg9f1f650mi5z8phx42r6hli"))))
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
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indexmap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07rxrqmryr1xfnmhrjlz8ic6jw28v6h5cig3ws2c9d0wifhy2c6m"))))
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
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "configparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dwjni8z9v26ysn7yqw3ickvqbrwjd0cv1ag20manlia990nxrg0"))))
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
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap-verbosity-flag" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06f0myl6chqvyf9dpv3ydblqp8sjrkwwm0nai8vzn33rbl0vpzg5"))))
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
    (version "0.4.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chrono" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f6vg67pipm8cziad2yms6a639pssnvysk1m05dd9crymmdnhb3z"))))
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
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "speakersafetyd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "114134b2hf6bqp2zncnkikhrhg118mk028f5p5axcdgclzpiq24p"))))
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
     (list alsa-lib))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/AsahiLinux/speakersafetyd/")
    (synopsis "Speaker protection daemon for embedded Linux systems")
    (description "Speaker protection daemon for embedded Linux systems")
    (license license:expat)))

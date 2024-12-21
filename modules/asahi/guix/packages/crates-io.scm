(define-module (asahi guix packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public rust-asahi-alsa
  (package
    (inherit (replace-alsa-lib rust-alsa-0.8))
    (name "rust-asahi-alsa")))

(define-public rust-asahi-bless-0.4
  (package
    (name "rust-asahi-bless")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asahi-bless" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lf6iq6imb2x79avh40r6inmqm893sv79mmbdz096k77v1ajxv9r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-apple-nvram" ,rust-apple-nvram-0.3)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-gpt" ,rust-gpt-3)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/WhatAmISupposedToPutHere/asahi-nvram")
    (synopsis "Tool to select active boot partition on ARM Macs")
    (description "This package provides a tool to select active boot partition on ARM
Macs.")
    (license license:expat)))

(define-public rust-asahi-btsync-0.2
  (package
    (name "rust-asahi-btsync")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asahi-btsync" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0762v4ajlsk0grpzk1gvylxpix0jipwcrasq59b3r2hsp8r7icgi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-apple-nvram" ,rust-apple-nvram-0.3)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-rust-ini" ,rust-rust-ini-0.18))))
    (home-page "https://github.com/WhatAmISupposedToPutHere/asahi-nvram")
    (synopsis "Tool to sync Bluetooth pairing keys with MacOS on ARM Macs")
    (description "This package provides a tool to sync Bluetooth pairing keys with MacOS
on ARM Macs.")
    (license license:expat)))

(define-public rust-gpt-3
  (package
    (name "rust-gpt")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gpt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sddnv1lqs0yfjzzkyn6agwm4a1ij3xvrz8cdrsvk4wc3cryg0w2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-2)
                       ("rust-crc" ,rust-crc-3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/Quyzi/gpt")
    (synopsis "Rust library to work with GPT partition tables.")
    (description "This package provides a pure-Rust library to work with GPT partition
tables.")
    (license license:expat)))

(define-public rust-apple-nvram-0.3
  (package
    (name "rust-apple-nvram")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "apple-nvram" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1avymr2x9hr0qrqs5881x4vgmk819pjrldpkf9kia0s8dxphbhc6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-adler32" ,rust-adler32-1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-nix" ,rust-nix-0.26))))
    (home-page "https://github.com/WhatAmISupposedToPutHere/asahi-nvram")
    (synopsis "library to parse and write apple-formatted nvram entries")
    (description "This package provides a library to parse and write apple-formatted
nvram entries.")
    (license license:expat)))

(define-public rust-asahi-nvram-0.2
  (package
    (name "rust-asahi-nvram")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asahi-nvram" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k1slgayjxf4pz6f44gckqwh44wk1n0iln6s3008x8rxxvsmxqac"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-apple-nvram" ,rust-apple-nvram-0.3)
                       ("rust-clap" ,rust-clap-3))))
    (home-page "https://github.com/WhatAmISupposedToPutHere/asahi-nvram")
    (synopsis "Tool to read and write nvram variables on ARM Macs")
    (description
     "This package provides a tool to read and write nvram variables on ARM Macs")
    (license license:expat)))

(define-public rust-prettyplease-0.2
  (package
    (name "rust-prettyplease")
    (version "0.2.22")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "prettyplease" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fpsyn4x1scbp8ik8xw4pfh4jxfm5bv7clax5k1jcd5vzd0gk727"))))
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
                       ("rust-lv2" ,rust-lv2-0.6))

       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-lv2
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "LIBDIR" (string-append (assoc-ref outputs "out") "/lib"))
             (invoke "make" "install"))))))
    (home-page "https://github.com/chadmed/bankstown")
    (synopsis "Barebones bass enhancer")
    (description "Halfway-decent three-stage psychoacoustic bass approximation.")
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    (license license:expat)))

(define-public rust-clap-verbosity-flag-2
  (package
    (name "rust-clap-verbosity-flag")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap-verbosity-flag" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vb4amgfp20mkfglhx2m4iwxibc74apf9srcbvvjyrh7327176g0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/clap-rs/clap-verbosity-flag")
    (synopsis "Easily add a `--verbose` flag to CLIs using Clap")
    (description "Easily add a `--verbose` flag to CLIs using Clap")
    (license (list license:expat license:asl2.0))))

(define-public rust-speakersafetyd-1
  (package
    (name "rust-speakersafetyd")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "speakersafetyd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dvyj194niz5i4rldsqvjmz8j7df9w9qpvf9rwg3vsnzc2mjh0zg"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-inputs `(("rust-alsa" ,rust-alsa-0.8)
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
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda _
              (substitute* "src/main.rs"
                (("/usr/local") #$output))))
          (add-after 'unpack 'remove-systemd-udev-rules
            (lambda _
              (substitute* "95-speakersafetyd.rules"
                ((".*SYSTEMD_WANTS.*") ""))))
          (add-after 'install 'install-data
            (lambda _
              (setenv "BINDIR" (string-append #$output "/bin"))
              (setenv "UNITDIR" (string-append #$output "/lib/systemd/system"))
              (setenv "UDEVDIR" (string-append #$output "/lib/udev/rules.d"))
              (setenv "TMPFILESDIR" (string-append #$output "/usr/lib/tmpfiles.d"))
              (setenv "SHAREDIR" (string-append #$output "/usr/share"))
              (setenv "VARDIR" (string-append #$output "/var"))
              (invoke "make" "install-data"))))))
    (inputs (list alsa-lib))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/AsahiLinux/speakersafetyd/")
    (synopsis "Speaker protection daemon")
    (description "Speakersafetyd is a userspace daemon written in Rust that
implements an analogue of the Texas Instruments Smart Amp speaker protection
model.")
    (license license:expat)))

(define-public rust-asahi-wifisync-0.2
  (package
    (name "rust-asahi-wifisync")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asahi-wifisync" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1l46x4g4p1s5qapi6450dfh5d29jcawpl3incdz2m6any0ilxddh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-apple-nvram" ,rust-apple-nvram-0.3)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-rust-ini" ,rust-rust-ini-0.18))))
    (home-page "https://github.com/WhatAmISupposedToPutHere/asahi-nvram")
    (synopsis "Tool to sync Wifi passwords with MacOS on ARM Macs")
    (description "This package provides a tool to sync Wifi passwords with MacOS on ARM
Macs.")
    (license license:expat)))

(define-public rust-udev-0.9
  (package
    (name "rust-udev")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "udev" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10dxy4vd16mkq0xi24d3nwgiv007qhrmry493j9nj5szp6bw3mg3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-io-lifetimes" ,rust-io-lifetimes-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libudev-sys" ,rust-libudev-sys-0.1)
                       ("rust-mio" ,rust-mio-0.7)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-mio" ,rust-mio-1)
                       ("rust-mio" ,rust-mio-0.6)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/Smithay/udev-rs")
    (synopsis "libudev bindings for Rust")
    (description "This package provides libudev bindings for Rust.")
    (license license:expat)))

(define-public rust-tokio-stream-0.1
  (package
    (name "rust-tokio-stream")
    (version "0.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ix0770hfp4x5rh5bl7vsnr3d4iz4ms43i522xw70xaap9xqv9gc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities to work with `Stream` and `tokio`.")
    (description
     "This package provides Utilities to work with `Stream` and `tokio`.")
    (license license:expat)))

(define-public rust-procfs-core-0.17
  (package
    (name "rust-procfs-core")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "procfs-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v0jdbyc1rq1x22m0wn7n4iq4h86gdls38wqfg06zc29hcnz1793"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/eminence/procfs")
    (synopsis
     "Data structures and parsing for the linux procfs pseudo-filesystem")
    (description
     "This package provides Data structures and parsing for the linux procfs pseudo-filesystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-procfs-0.17
  (package
    (name "rust-procfs")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "procfs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17swyjqinpb745f07dpdi7c8q37hxvhx9xmmsi2dhxaj2kc74nyc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-procfs-core" ,rust-procfs-core-0.17)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/eminence/procfs")
    (synopsis "Interface to the linux procfs pseudo-filesystem")
    (description
     "This package provides Interface to the linux procfs pseudo-filesystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-krun-sys-1
  (package
    (name "rust-krun-sys")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "krun-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13z226ynks7psa2z3pk58l5znay4861v0k0qfnzcvwd6l1bvg99d"))))
    (build-system cargo-build-system)
    (arguments
     (list #:skip-build? #t
           #:cargo-inputs `(("rust-bindgen" ,rust-bindgen-0.69))
           ;; #:phases
           ;; #~(modify-phases %standard-phases
           ;;     (replace 'install
           ;;       (lambda _
           ;;         (setenv "DESTDIR" #$output)
           ;;         (invoke "pwd")
           ;;         (invoke "find")
           ;;         (invoke "make" "install"))))
           ))
    (home-page "https://github.com/containers/libkrun")
    (synopsis "Rust bindings for libkrun")
    (description "This package provides Rust bindings for libkrun.")
    (license license:asl2.0)))

(define-public rust-input-linux-sys-0.9
  (package
    (name "rust-input-linux-sys")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "input-linux-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0957hf6sqlp9kw5vqxm80pklk9q745zhprpmdrbhmbqficjb54bv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.29))))
    (home-page "https://github.com/arcnmx/input-linux-sys-rs")
    (synopsis "Bindings for <linux/{u,}input.h>")
    (description "This package provides Bindings for <linux/{u,}input.h>.")
    (license license:expat)))

(define-public rust-input-linux-0.7
  (package
    (name "rust-input-linux")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "input-linux" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q2227n0pyqffhgi4bn4h90y91rz4cfll8v9ra15bfc83j1c9s5p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-input-linux-sys" ,rust-input-linux-sys-0.9)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tokio-util" ,rust-tokio-util-0.6)
                       ("rust-tokio-util" ,rust-tokio-util-0.7))))
    (home-page "https://github.com/arcnmx/input-linux-rs")
    (synopsis "evdev and uinput")
    (description "This package provides evdev and uinput.")
    (license license:expat)))

(define-public rust-env-logger-0.11
  (package
    (name "rust-env-logger")
    (version "0.11.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "env_logger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q30cqb2dfs3qrs0s30qdmqwi7n2gz4pniwd8a9gvhygwgcf7bnw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-env-filter" ,rust-env-filter-0.1)
                       ("rust-humantime" ,rust-humantime-2)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/rust-cli/env_logger")
    (synopsis
     "logging implementation for `log` which is configured via an environment
variable.")
    (description
     "This package provides a logging implementation for `log` which is configured via
an environment variable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bpaf-derive-0.5
  (package
    (name "rust-bpaf-derive")
    (version "0.5.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bpaf_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08l7llwp1g6fvrx1j9wyvgs7fii5jcg0jqbpq27pz9mbwv3xk5fg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/pacak/bpaf")
    (synopsis "Derive macros for bpaf Command Line Argument Parser")
    (description
     "This package provides Derive macros for bpaf Command Line Argument Parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bpaf-0.9
  (package
    (name "rust-bpaf")
    (version "0.9.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bpaf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ykp9k3a53rggixmr91j9w37zlsj120gps4nvhnzmhkdhrs53zah"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bpaf-derive" ,rust-bpaf-derive-0.5)
                       ("rust-owo-colors" ,rust-owo-colors-3)
                       ("rust-supports-color" ,rust-supports-color-2))))
    (home-page "https://github.com/pacak/bpaf")
    (synopsis "simple Command Line Argument Parser with parser combinators")
    (description
     "This package provides a simple Command Line Argument Parser with parser
combinators.")
    (license (list license:expat license:asl2.0))))

;; wrapper.h:1:10: fatal error: 'libkrun.h' file not found
(define-public rust-muvm-0.2
  (package
    (name "rust-muvm")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "muvm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0b6i8s8yzk29lyrbs29y1xni72vn3w18j54k2jm738qzj13y6jhz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bpaf" ,rust-bpaf-0.9)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-env-logger" ,rust-env-logger-0.11)
                       ("rust-input-linux" ,rust-input-linux-0.7)
                       ("rust-input-linux-sys" ,rust-input-linux-sys-0.9)
                       ("rust-krun-sys" ,rust-krun-sys-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.29)
                       ("rust-procfs" ,rust-procfs-0.17)
                       ("rust-rustix" ,rust-rustix-0.38)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-udev" ,rust-udev-0.9)
                       ("rust-uuid" ,rust-uuid-1))))
    (native-inputs (list clang llvm pkg-config))
    (inputs (list eudev))
    (home-page "https://github.com/AsahiLinux/muvm")
    (synopsis "Run programs from your system in a microVM")
    (description "This package provides a way to run programs from your system in a
@code{microVM}.")
    (license license:expat)))

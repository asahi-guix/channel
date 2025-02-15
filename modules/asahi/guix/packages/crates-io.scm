(define-module (asahi guix packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu packages crates-audio)
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

(define-public rust-asahi-bless-0.4
  (package
    (name "rust-asahi-bless")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asahi-bless" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bfxi0pd9z6rszvma8vfjyzhgbbawl3djyfpb4g0diwxnb6r8as6"))))
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

(define-module (asahi guix packages linux)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define config->string
  (@@ (gnu packages linux) config->string))

(define* (make-asahi-linux-source version revision hash)
  (let ((commit (string-append "asahi-" version "-" revision)))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/AsahiLinux/linux.git")
            (commit commit)))
      (file-name (git-file-name "linux-source" commit))
      (sha256 (base32 hash)))))

(define* (make-asahi-linux
          name
          #:key
          (defconfig (local-file "defconfig"))
          (extra-options '())
          (extra-version #f)
          (hash "0h018yj414n9js701mcsl25nhnp9h0il83f90jfd4y1lccjnv4nh")
          (linux linux-libre-arm64-generic)
          (revision "1")
          (version "6.12.4"))
  (let ((base (customize-linux
               #:configs (config->string (or extra-options '()))
               #:defconfig defconfig
               #:linux (package/inherit linux (version (string-append version "-" revision)))
               #:name name
               #:source (make-asahi-linux-source version revision hash)
               #:extra-version extra-version)))
    (package
      (inherit base)
      (name name)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-before 'configure 'configure-bindgen
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((bindgen (false-if-exception
                                  (search-input-file inputs "bin/bindgen"))))
                    (when bindgen (setenv "BINDGEN" bindgen)))))
              (add-before 'configure 'configure-libclang
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((clang (assoc-ref inputs "clang")))
                    (when clang
                      (setenv "CC" "clang")
                      (setenv "LIBCLANG_PATH" (string-append clang "/lib"))))))
              (add-before 'configure 'configure-rustc
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((rustc (false-if-exception (search-input-file inputs "bin/rustc"))))
                    (when rustc (setenv "RUSTC" rustc)))))
              (add-before 'configure 'configure-rust-src
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((source (false-if-exception
                                 (search-input-directory inputs "lib/rustlib/src/rust/library"))))
                    (when source (setenv "RUST_LIB_SRC" source)))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend cpio
                  python
                  rust
                  `(,rust "rust-src")
                  `(,rust "tools")
                  rust-bindgen-cli
                  zstd)))
      (home-page "https://asahilinux.org")
      (synopsis "Linux on Apple Silicon")
      (description "Asahi Linux is a project and community with the goal of porting Linux
to Apple Silicon Macs, starting with the 2020 M1 Mac Mini, MacBook
Air, and MacBook Pro."))))

(define-public asahi-linux
  (make-asahi-linux "asahi-linux"))

(define-public asahi-alsa-ucm-conf
  (package
    (name "asahi-alsa-ucm-conf")
    (version "5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AsahiLinux/alsa-ucm-conf-asahi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "072gw5mbi8wgjh8f8gddqcf8pn3fsspsl4zd639ggb0lkb7hv9bm"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("ucm2" "share/alsa/ucm2"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'add-alsa-ucm-conf
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (alsa-ucm-conf (assoc-ref inputs "alsa-ucm-conf")))
                (for-each (lambda (dir)
                            (let ((path (format #f "/share/alsa/~a" dir)))
                              (copy-recursively
                               (string-append alsa-ucm-conf path)
                               (string-append out path)
                               #:follow-symlinks? #t)))
                          '("ucm" "ucm2"))))))))
    (native-inputs
     `(("alsa-ucm-conf" ,linux:alsa-ucm-conf)))
    (home-page "https://github.com/AsahiLinux/alsa-ucm-conf-asahi")
    (synopsis "The Advanced Linux Sound Architecture Use Case Manager")
    (description
     "This package contains Advanced Linux Sound Architecture Use Case Manager
configuration of audio input/output names and routing for specific audio
hardware.")
    (license license:bsd-3)))

(define replace-alsa-ucm-conf
  (package-input-rewriting/spec
   `(("alsa-ucm-conf" . ,(const asahi-alsa-ucm-conf)))))

(define-public asahi-alsa-lib
  (package
    (inherit (replace-alsa-ucm-conf linux:alsa-lib))
    (name "asahi-alsa-lib")))

(define-public replace-alsa-lib
  (package-input-rewriting/spec
   `(("alsa-lib" . ,(const asahi-alsa-lib)))))

(define-public asahi-alsa-utils
  (package
    (inherit (replace-alsa-ucm-conf linux:alsa-utils))
    (name "asahi-alsa-utils")))

(define-public asahi-alsa-plugins
  (package
    (inherit (replace-alsa-ucm-conf linux:alsa-plugins))
    (name "asahi-alsa-plugins")))

(define-public asahi-pipewire
  (let ((base (replace-alsa-ucm-conf linux:pipewire)))
    (package
      (inherit base)
      (name "asahi-pipewire")
      (inputs (modify-inputs (package-inputs base)
                (prepend lilv))))))

(define-public asahi-wireplumber
  (package
    (inherit (replace-alsa-ucm-conf linux:wireplumber))
    (name "asahi-wireplumber")))

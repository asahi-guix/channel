(define-module (asahi guix packages linux)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages rust)
  #:use-module (asahi guix packages rust-apps)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python)
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

(define (make-asahi-linux-source commit hash patches)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/AsahiLinux/linux.git")
          (commit commit)))
    (file-name (git-file-name "linux-source" commit))
    (patches patches)
    (sha256 (base32 hash))))

(define asahi-linux-source-6.9.12-1
  (make-asahi-linux-source
   "asahi-6.9.12-1" "13xvsp0mcny19bpqgp6m0kydhnv9648q3wlbg2zx4f2mwhdjn8rc"
   (list)))

(define* (make-asahi-linux name
                           #:key
                           (defconfig (local-file "defconfig.main"))
                           (extra-options '())
                           (extra-version #f)
                           (linux linux-libre-arm64-generic)
                           (source asahi-linux-source-6.9.12-1)
                           (version "6.9.12-asahi"))
  (let ((base (customize-linux
               #:configs (config->string (or extra-options '()))
               #:defconfig defconfig
               #:linux (package/inherit linux (version version))
               #:name name
               #:source source
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
                  (let ((bindgen (assoc-ref inputs "rust-bindgen-cli")))
                    (when bindgen
                      (setenv "BINDGEN"
                              (string-append bindgen "/bin/bindgen"))))))
              (add-before 'configure 'configure-libclang
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((clang (assoc-ref inputs "clang")))
                    (when clang
                      (setenv "CC" "clang")
                      (setenv "LIBCLANG_PATH" (string-append clang "/lib"))))))
              (add-before 'configure 'configure-rustc
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((rust (assoc-ref inputs "rust")))
                    (when rust
                      (setenv "RUSTC" (string-append rust "/bin/rustc"))))))
              (add-before 'configure 'configure-rust-src
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((rust-src (assoc-ref inputs "rust-src")))
                    (when rust-src
                      (setenv "RUST_LIB_SRC"
                              (string-append rust-src
                                             "/lib/rustlib/src/rust/library"))))))))))
      (home-page "https://asahilinux.org")
      (synopsis "Linux on Apple Silicon")
      (description "Asahi Linux is a project and community with the goal of porting Linux
to Apple Silicon Macs, starting with the 2020 M1 Mac Mini, MacBook
Air, and MacBook Pro."))))

(define-public asahi-linux
  (make-asahi-linux "asahi-linux"))

(define-public asahi-linux-edge
  (let ((base (make-asahi-linux
               "asahi-linux-edge"
               #:defconfig (local-file "defconfig.edge"))))
    (package/inherit base
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend clang
                  llvm
                  python
                  rust
                  rust-bindgen-cli-0.69
                  rust-src
                  zstd))))))

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

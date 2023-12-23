(define-module (asahi guix packages linux)
  #:use-module (asahi guix packages crates-io)
  #:use-module (asahi guix packages rust)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define config->string
  (@@ (gnu packages linux) config->string))

(define (make-asahi-linux-source commit hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/AsahiLinux/linux.git")
          (commit commit)))
    (file-name (git-file-name "linux-source" commit))
    (sha256
     (base32 hash))))

(define asahi-linux-source-6.6-14
  (make-asahi-linux-source
   "asahi-6.6-14" "0bi6s2nhdibf1igkwn7ynzfjgzrw0jphpmkfmdgwavf8fpcmf9zv"))

(define* (make-asahi-linux name
                           #:key
                           (defconfig (local-file "defconfig.main"))
                           (extra-options '())
                           (extra-version #f)
                           (linux linux-libre-arm64-generic)
                           (source asahi-linux-source-6.6-14)
                           (version "6.6.0"))
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

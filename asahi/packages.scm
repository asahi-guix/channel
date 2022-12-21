(define-module (asahi packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rust)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (nongnu packages linux)
  #:use-module (srfi srfi-1))

(define (make-asahi-linux name config)
  (package
    (inherit linux-arm64-generic)
    (name name)
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/linux.git")
             (commit "6ecde4985ffc8490eb3dbc9303d338b8872e3991")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05r2i3dnwa9v35x93p6r6ixnf456annfx498jgmviwl53jkxi1qc"))))
    (native-inputs
     `(("kconfig" ,config)
       ("zstd" ,zstd)
       ,@(alist-delete "kconfig" (package-native-inputs linux-arm64-generic))))
    (home-page "https://asahilinux.org")
    (synopsis "Linux on Apple Silicon")
    (description "Asahi Linux is a project and community with the goal of porting Linux
to Apple Silicon Macs, starting with the 2020 M1 Mac Mini, MacBook
Air, and MacBook Pro.")))

(define-public asahi-linux
  (make-asahi-linux "asahi-linux" (local-file "kernel.config")))

(define-public asahi-linux-edge
  (make-asahi-linux "asahi-linux-edge" (local-file "kernel.edge.config")))

(define-public asahi-m1n1
  (package
    (name "asahi-m1n1")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/m1n1.git")
             (commit "fb36e302363cbe505da856f127bc0ae5e4a11257")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fm8vhdi75nfwm3n9wy5hrg26wa10h2bgba3250jfv2lg62rsd50"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "RELEASE" "1")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/usr/lib/")))
               (mkdir-p dir)
               (copy-file "build/m1n1.bin" (string-append dir "m1n1.bin")))))
         (delete 'check))))
    (home-page "https://github.com/AsahiLinux/m1n1")
    (synopsis "Experimentation playground for Apple Silicon")
    (description "A bootloader and experimentation playground for Apple Silicon")
    (license license:expat)))

(define-public asahi-fwextract
  (package
    (name "asahi-fwextract")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/asahi-installer.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kj9ycy3f34fzm9bnirlcw9zm2sgipwrqzphdg5k099rbjbc7zmj"))))
    (build-system python-build-system)
    (home-page "https://github.com/AsahiLinux/asahi-installer")
    (synopsis "Asahi Linux firmware extractor")
    (description "The Asahi Linux firmware extraction tool")
    (license license:expat)))

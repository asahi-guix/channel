(define-module (asahi packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rust)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (nongnu packages linux)
  #:use-module (srfi srfi-1))

(define-public asahi-linux
  (package
    (inherit linux-arm64-generic)
    (name "asahi-linux")
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
     `(("kconfig" ,(local-file "kernel.config"))
       ("zstd" ,zstd)
       ,@(alist-delete "kconfig" (package-native-inputs linux-arm64-generic))))
    (home-page "https://asahilinux.org")
    (synopsis "Linux on Apple Silicon")
    (description "Asahi Linux is a project and community with the goal of porting Linux
to Apple Silicon Macs, starting with the 2020 M1 Mac Mini, MacBook
Air, and MacBook Pro.")))

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

;; (define-public u-boot-apple-m1
;;   (let ((base (make-u-boot-package "puma-rk3399" "aarch64-linux-gnu")))
;;     (package
;;       (inherit (make-u-boot-package "apple_m1" "aarch64-linux-gnu"))
;;       (name "u-boot-apple-m1")
;;       (version "2022.10-1")
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri (git-reference
;;                (url "https://github.com/AsahiLinux/u-boot.git")
;;                (commit "54409548c3aa8cf4820f1bda69a26bb603a0a5a4")))
;;          (file-name (git-file-name name version))
;;          (sha256
;;           (base32 "1m1w6ajzsfpb59abncz3sa9b1waqjsnh2vm7js2n22xiw4km7nzx"))))
;;       (native-inputs (list asahi-m1n1))
;;       (native-inputs
;;        `(,@(if (not (same-arch?))
;;                `(("cross-gcc" ,(cross-gcc triplet))
;;                  ("cross-binutils" ,(cross-binutils triplet)))
;;                `())
;;          ,@(package-native-inputs base))))))

;; (define-public u-boot-apple-m1
;;   (let ((base (make-u-boot-package "apple_m1" "aarch64-linux-gnu")))
;;     (package
;;       (inherit base)
;;       (name "u-boot-apple-m1")
;;       (version "2022.10-1")
;;       (source
;;        (origin
;;          (method git-fetch)
;;          (uri (git-reference
;;                (url "https://github.com/AsahiLinux/u-boot.git")
;;                (commit "54409548c3aa8cf4820f1bda69a26bb603a0a5a4")))
;;          (file-name (git-file-name name version))
;;          (sha256
;;           (base32 "1m1w6ajzsfpb59abncz3sa9b1waqjsnh2vm7js2n22xiw4km7nzx"))))
;;       (inputs
;;        `(("asahi-m1n1" ,asahi-m1n1)
;;          ,@(package-inputs base)))
;;       (native-inputs
;;        `(("asahi-m1n1" ,asahi-m1n1)
;;          ,@(package-native-inputs base))))))

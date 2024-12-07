(define-module (asahi guix packages bootloader)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public asahi-bootlogo
  (package
    (name "asahi-bootlogo")
    (version "0.0.1")
    (source (local-file "../files/bootlogo.svg"))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((source (assoc-ref inputs "source"))
                    (target (string-append #$output "/share/asahi-m1n1")))
                (mkdir-p target)
                (for-each (lambda (width)
                            (invoke "convert"
                                    "-background" "none"
                                    "-resize" (format #f "~ax~a!" width width)
                                    source
                                    (format #f "~a/bootlogo_~a.png" target width)))
                          (list 128 256))))))))
    (home-page "https://guix.gnu.org/")
    (native-inputs (list imagemagick inkscape))
    (synopsis "Asahi Guix boot logo pacakge")
    (description "The package providing the Asahi Guix boot logo.")
    (license license:expat)))

(define-public asahi-m1n1
  (package
    (name "asahi-m1n1")
    (version "1.4.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/m1n1")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kg29s1rnlq0plj4swg6nc4vpfmk0gd6zw96lzh538fdc0423pka"))))
    (build-system gnu-build-system)
    (supported-systems (list "aarch64-linux"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'bootlogo
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bootlogo (assoc-ref inputs "asahi-bootlogo")))
                (when bootlogo
                  (let ((source (string-append bootlogo "/share/asahi-m1n1")))
                    (chdir "data")
                    (for-each
                     (lambda (width)
                       (let ((source (format #f "~a/bootlogo_~a.png" source width))
                             (target (format #f "bootlogo_~a.png" width)))
                         (when (file-exists? source)
                           (delete-file target)
                           (copy-file source target))))
                     (list 128 256))
                    (invoke "sh" "makelogo.sh")
                    (chdir ".."))))))
          (replace 'configure
            (lambda _
              (setenv "RELEASE" "1")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((dir (string-append (assoc-ref outputs "out") "/libexec/")))
                (mkdir-p dir)
                (copy-file "build/m1n1.bin" (string-append dir "m1n1.bin")))))
          ;; There are no tests
          (delete 'check))))
    (native-inputs (list asahi-bootlogo imagemagick))
    (home-page "https://github.com/AsahiLinux/m1n1")
    (synopsis "Boot loader and experimentation playground for Apple Silicon")
    (description "m1n1 is the bootloader developed by the Asahi Linux project to bridge
the Apple (XNU) boot ecosystem to the Linux boot ecosystem.")
    (license license:expat)))

(define-public asahi-u-boot
  (let ((base (make-u-boot-package "apple_m1" "aarch64-linux-gnu"))
        (commit "4b6e0ff9087fba59f189b8f0ca3c3f6cf26e704f"))
    (package/inherit base
      (version (git-version "2024.10-1" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AsahiLinux/u-boot")
               (commit commit)))
         (file-name (git-file-name (package-name base) version))
         (sha256
          (base32 "1sj4cixzmr7m620dj5gm65w8dqj1frn57xxqbc5d219533wfvmc2"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (delete 'disable-tools-libcrypto)))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend libressl))))))

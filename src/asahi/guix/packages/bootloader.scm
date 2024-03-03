(define-module (asahi guix packages bootloader)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootloaders)
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
    (source (local-file "../files/asahi-bootlogo.svg"))
    (build-system copy-build-system)
    (home-page "https://www.gnu.org/graphics/heckert_gnu.html")
    (synopsis "Asahi Guix boot logo")
    (description "A modified version of the famous GNU logo for the U-Boot.")
    (license license:expat)))

(define-public asahi-m1n1
  (package
    (name "asahi-m1n1")
    (version "1.4.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/m1n1")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j62aj5gzmldgkyikcvdg5rxym8mpswgbf00bk1h8mdccc6qiwnz"))))
    (build-system gnu-build-system)
    (supported-systems (list "aarch64-linux"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'bootlogo
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bootlogo (assoc-ref inputs "asahi-bootlogo"))
                     (bootlogo-input (string-append bootlogo "/asahi-bootlogo.svg")))
                (when (file-exists? bootlogo-input)
                  (chdir "data")
                  (delete-file "bootlogo_128.png")
                  (invoke "convert"
                          "-background" "none"
                          "-resize" "128x128!"
                          bootlogo-input
                          "bootlogo_128.png")
                  (delete-file "bootlogo_256.png")
                  (invoke "convert"
                          "-background" "none"
                          "-resize" "256x256!"
                          bootlogo-input
                          "bootlogo_256.png")
                  (invoke "sh" "makelogo.sh")
                  (chdir "..")))))
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

(define-public u-boot-apple-m1
  (let ((base (make-u-boot-package "apple_m1" "aarch64-linux-gnu"))
        (commit "71f6d0feb47836614752a0e30a1b7339b9e15656"))
    (package/inherit base
      (version (git-version "2023.07.02-4" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AsahiLinux/u-boot")
               (commit commit)))
         (file-name (git-file-name (package-name base) version))
         (sha256
          (base32 "0dv242kxz3zw9ly3rwdj87bfh4byqqrmgy9a5605vhb04c9s92ik"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (delete 'disable-tools-libcrypto)))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend libressl))))))

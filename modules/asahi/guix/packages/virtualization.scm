(define-module (asahi guix packages virtualization)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages jemalloc)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public fex-emu
  (package
    (name "fex-emu")
    (version "2410")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.como/FEX-Emu/FEX")
                    (commit (string-append "FEX-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1pi1lib9n0j3vn68hnqgjcq13qr3s0mnfvn4kbr5yirls806iyah"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; (add-after 'unpack 'delete-external
          ;;   (lambda _
          ;;     (delete-file-recursively "External")))
          (add-before 'configure 'configure-cc
            (lambda _
              (setenv "CC" "clang"))))))
    (native-inputs (list clang-18 llvm-18 ninja))
    (inputs (list jemalloc xxhash))
    (home-page "https://github.com/FEX-Emu/FEX")
    (synopsis "A fast usermode x86 and x86-64 emulator for Arm64 Linux")
    (description "FEX allows you to run x86 and x86-64 binaries on an AArch64 host,
similar to qemu-user and box86. It has native support for a rootfs
overlay, so you don't need to chroot, as well as some thunklibs so it
can forward things like GL to the host. FEX presents a Linux 5.0+
interface to the guest, and supports only AArch64 as a host.")
    (license license:expat)))

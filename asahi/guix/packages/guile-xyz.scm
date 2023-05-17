(define-module (asahi guix packages guile-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public guile-asahi-guix
  (let ((commit "eaa6a6c67fa59e7e2d56392e12b651d8c5a004c0")
        (revision "1"))
    (package
      (name "guile-asahi-guix")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/r0man/asahi-guix.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0n4skhx98kng9xg75fkl3rjvln0xm84mvp60q1l70417lpr1wllr"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf
             automake
             guile-hall
             guix
             pkg-config
             texinfo
             util-linux))
      (inputs (list guile-3.0-latest util-linux))
      (synopsis "Asahi Guix")
      (description "Asahi Linux on GNU Guix")
      (home-page "https://github.com/r0man/asahi-guix")
      (license license:gpl3+))))

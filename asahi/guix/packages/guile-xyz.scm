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
  (let ((commit "b18980114d7c798d54284339dcb4cf7ec9f4a25e")
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
                  "1smnm2f373f2c39b5827nwx9s9wg5nq9bq32yyf0hpq95cah1q61"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'prepare
              (lambda* (#:key inputs #:allow-other-keys)
                (setenv "PATH" (string-append
                                (getenv "PATH") ":"
                                (string-append
                                 (assoc-ref inputs "util-linux")
                                 "/sbin")))
                (invoke "hall" "build" "--execute"))))))
      (native-inputs
       (list autoconf
             automake
             guile-hall
             pkg-config
             texinfo
             util-linux))
      (inputs (list guile-3.0-latest util-linux))
      (propagated-inputs (list util-linux guix))
      (synopsis "Asahi Guix")
      (description "Asahi Linux on GNU Guix")
      (home-page "https://github.com/r0man/asahi-guix")
      (license license:gpl3+))))

(use-modules
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages disk)
 (gnu packages gettext)
 (gnu packages gnupg)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages linux)
 (gnu packages package-management)
 (gnu packages pkg-config)
 (gnu packages ssh)
 (gnu packages texinfo)
 (guix build-system gnu)
 (guix build-system guile)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (srfi srfi-1))

(define-public guile-asahi-guix
  (package
    (name "guile-asahi-guix")
    (version "0.0.1")
    (source
     (local-file
      (dirname (current-filename))
      #:recursive? #t
      #:select?
      (λ (file stat)
        (not (any (lambda (my-string)
                    (string-contains file my-string))
                  (list ".git" ".dir-locals.el" "guix.scm"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           gnu-gettext
           guile-ares-rs
           guile-gcrypt
           guile-git
           guile-hall
           guile-json-4
           guile-next
           guile-ssh
           guix
           parted
           pkg-config
           texinfo))
    (inputs (list guile-next util-linux))
    (synopsis "Asahi Guix")
    (description "Asahi Linux on GNU Guix")
    (home-page "https://codeberg.org/asahi-guix/channel")
    (license license:gpl3+)))

guile-asahi-guix

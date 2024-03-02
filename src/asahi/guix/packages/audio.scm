(define-module (asahi guix packages audio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public asahi-audio
  (package
    (name "asahi-audio")
    (version "1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chadmed/asahi-audio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "16ggnqjz31qfpjgnbl5s377lr0yp36aam7kap0k2aj2hij4q4nv4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "conf/85-asahi-policy.lua"
                 (("/usr/share/asahi-audio")
                  (string-append out "/share/asahi-audio")))
               (substitute* (find-files "firs" "\\.json$")
                 (("/usr/share/asahi-audio")
                  (string-append out "/share/asahi-audio"))))))
         (delete 'configure)
         (delete 'check))))
    (home-page "https://github.com/chadmed/asahi-audio")
    (synopsis "Linux audio configuration for Apple Silicon Macs")
    (description "Linux userspace audio configuration for Apple Silicon Macs.")
    (license license:expat)))

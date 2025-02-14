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
    (version "2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chadmed/asahi-audio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1s5bq4i89m80ssxn9lm1avyj9x8hhvrk80hbwzdhs49zy3d1zqdv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "conf/wireplumber.conf"
                 (("/usr/share/asahi-audio")
                  (string-append out "/share/asahi-audio"))
                 (("device/asahi-limit-volume.lua")
                  (string-append out "/share/wireplumber/scripts/device/asahi-limit-volume.lua")))
               (substitute* (find-files "firs" "\\.json$")
                 (("/usr/share/asahi-audio")
                  (string-append out "/share/asahi-audio"))))))
         (delete 'configure)
         (delete 'check))))
    (home-page "https://github.com/chadmed/asahi-audio")
    (synopsis "Linux audio configuration for Apple Silicon Macs")
    (description "Linux userspace audio configuration for Apple Silicon Macs.")
    (license license:expat)))

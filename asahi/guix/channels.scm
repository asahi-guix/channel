(define-module (asahi guix channels)
  #:use-module (guix channels)
  #:export (channels))

(define channels
  (list (channel
         (name 'asahi)
         (url "https://github.com/r0man/asahi-guix.git")
         ;; (branch "main")
         (branch "manual-installation")
         (introduction
          (make-channel-introduction
           "d8f93a8a57be3e38f13916b4e5f3e1ad5ccee9de"
           (openpgp-fingerprint
            "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))

        (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (branch "master")
         (commit "7833acab0da02335941974608510c02e2d1d8069")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

channels

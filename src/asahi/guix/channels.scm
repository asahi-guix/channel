(define-module (asahi guix channels)
  #:use-module (guix channels)
  #:export (channels))

(define channels
  (list (channel
         (name 'asahi)
         (url "https://github.com/r0man/asahi-guix.git")
         (branch "main")
         (introduction
          (make-channel-introduction
           "d8f93a8a57be3e38f13916b4e5f3e1ad5ccee9de"
           (openpgp-fingerprint
            "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))

        (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (branch "master")
         (commit "5b700945fb0b33eec410de8979cae2fbf0d4f118")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

channels

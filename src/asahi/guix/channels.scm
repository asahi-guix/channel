(define-module (asahi guix channels)
  #:use-module (guix channels)
  #:export (asahi-guix-channel
            asahi-guix-next-channel
            channels
            guix-channel))

(define asahi-guix-channel
  (channel
   (name 'asahi-guix)
   (branch "main")
   (url "https://github.com/asahi-guix/channel.git")
   (introduction
    (make-channel-introduction
     "7677591b60ae62f76d8fcee392f0b249414442f6"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define asahi-guix-next-channel
  (channel
   (name 'asahi-guix-next)
   (branch "next")
   (url "https://github.com/asahi-guix/channel.git")
   (introduction
    (make-channel-introduction
     "7677591b60ae62f76d8fcee392f0b249414442f6"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define guix-channel
  (channel
   (name 'guix)
   (branch "main")
   (url "https://github.com/asahi-guix/guix.git")
   (introduction
    (make-channel-introduction
     "239d8d4f7f636e1d0c1e3928a57a3a123d672da5"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define channels
  (list asahi-guix-channel guix-channel))

channels

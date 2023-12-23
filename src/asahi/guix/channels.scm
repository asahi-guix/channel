(define-module (asahi guix channels)
  #:use-module (guix channels)
  #:export (asahi-guix-channel
            asahi-guix-next-channel
            channels
            guix-channel))

(define asahi-channel
  (channel
   (name 'asahi)
   (branch "main")
   (url "https://github.com/asahi-guix/channel.git")
   (introduction
    (make-channel-introduction
     "7677591b60ae62f76d8fcee392f0b249414442f6"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define asahi-next-channel
  (channel
   (name 'asahi-next)
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
     "f43bed7df1a6391a9260cd0f2b7c56c3a60b7d42"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define channels
  (list asahi-channel guix-channel))

channels

(define-module (asahi guix channels)
  #:use-module (guix channels)
  #:export (asahi-channel
            asahi-next-channel
            channels
            guix-channel))

(define asahi-channel
  (channel
   (name 'asahi)
   (branch "main")
   (url "https://github.com/asahi-guix/channel.git")
   (introduction
    (make-channel-introduction
     "3eeb493b037bea44f225c4314c5556aa25aff36c"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define asahi-next-channel
  (channel
   (name 'asahi-next)
   (branch "next")
   (url "https://github.com/asahi-guix/channel.git")
   (introduction
    (make-channel-introduction
     "3eeb493b037bea44f225c4314c5556aa25aff36c"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define guix-channel
  (channel
   (name 'guix)
   (url "https://github.com/asahi-guix/guix.git")
   (branch "main")
   (introduction
    (make-channel-introduction
     "f802d404b7229704190c821f89afd987be6a6905"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define channels
  (list asahi-channel guix-channel))

channels

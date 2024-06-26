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
   (url "https://github.com/asahi-guix/channel")
   (introduction
    (make-channel-introduction
     "3eeb493b037bea44f225c4314c5556aa25aff36c"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define asahi-next-channel
  (channel
   (name 'asahi-next)
   (branch "next")
   (url "https://github.com/asahi-guix/channel")
   (introduction
    (make-channel-introduction
     "3eeb493b037bea44f225c4314c5556aa25aff36c"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define guix-channel
  (channel
   (name 'guix)
   (url "https://github.com/asahi-guix/guix")
   (branch "main")
   (introduction
    (make-channel-introduction
     "8a1ce5f8a78ab104b1166f5b6bdc293e1cbf1b0a"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define channels
  (list asahi-channel guix-channel))

channels

(define-module (asahi guix packages pulseaudio)
  #:use-module ((gnu packages pulseaudio) #:prefix pulseaudio:)
  #:use-module (asahi guix packages linux)
  #:use-module (guix packages))


(define-public asahi-pulseaudio
  (package
    (inherit (replace-alsa-lib pulseaudio:pulseaudio))
    (name "asahi-pulseaudio")))

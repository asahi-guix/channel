(define-module (asahi guix transformations)
  #:use-module (asahi guix packages gl)
  #:use-module (asahi guix packages linux)
  #:use-module (guix packages))

(define-public replace-asahi
  (package-input-rewriting/spec
   `(("alsa-lib" . ,(const asahi-alsa-lib))
     ("alsa-ucm-conf" . ,(const asahi-alsa-ucm-conf))
     ("mesa" . ,(const asahi-mesa))
     ("mesa-headers" . ,(const asahi-mesa-headers))
     ("mesa-utils" . ,(const asahi-mesa-utils))
     ("pipewire" . ,(const asahi-pipewire))
     ("wireplumber" . ,(const asahi-wireplumber)))))

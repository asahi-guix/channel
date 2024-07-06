(define-module (asahi guix services sound)
  #:use-module (asahi guix packages audio)
  #:use-module (asahi guix packages crates-io)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu packages music)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (pipewire-configuration))

;; Alsa

(define-record-type* <alsa-configuration>
  alsa-configuration make-alsa-configuration alsa-configuration?
  (alsa-plugins alsa-configuration-alsa-plugins
                (default asahi-alsa-plugins))
  (pipewire alsa-configuration-pipewire
            (default asahi-pipewire))
  (extra-options alsa-configuration-extra-options
                 (default "")))

(define (alsa-pipewire-config-file config)
  (let ((pipewire (alsa-configuration-pipewire config)))
    #~(string-append #$pipewire "/share/alsa/alsa.conf.d/50-pipewire.conf")))

(define (alsa-pipewire-default-config-file config)
  (let ((pipewire (alsa-configuration-pipewire config)))
    #~(string-append #$pipewire "/share/alsa/alsa.conf.d/99-pipewire-default.conf")))

(define (alsa-etc-service config)
  `(("alsa/conf.d/50-pipewire.conf" ,(alsa-pipewire-config-file config))
    ("alsa/conf.d/50-pipewire-default.conf" ,(alsa-pipewire-default-config-file config))))

(define-public alsa-service-type
  (service-type
   (name 'alsa)
   (extensions
    (list (service-extension etc-service-type alsa-etc-service)))
   (default-value (alsa-configuration))
   (description "Configure low-level Linux sound support, ALSA.")))

;; Pipewire

(define-configuration/no-serialization pipewire-configuration
  (alsa-ucm-conf
   (file-like asahi-alsa-ucm-conf)
   "The Alsa Use Case Manager to use.")
  (asahi-audio
   (file-like asahi-audio)
   "The Asahi Linux Audio package to use.")
  (bankstown
   (file-like rust-bankstown)
   "The Bass enhancer package to use.")
  (lsp-plugins
   (file-like lsp-plugins)
   "The Lsp Plugins package to use.")
  (pipewire
   (file-like asahi-pipewire)
   "The PipeWire package to use.")
  (wireplumber
   (file-like asahi-wireplumber)
   "The WirePlumber package to use.")
  (enable-pulseaudio?
   (boolean #t)
   "When true, enable PipeWire's PulseAudio emulation support, allowing
PulseAudio clients to use PipeWire transparently."))

(define (alsa-config-ucm2 config)
  #~(string-append #$(pipewire-configuration-alsa-ucm-conf config)
                   "/share/alsa/ucm2"))

(define (lv2-path config)
  #~(string-append
     #$(pipewire-configuration-bankstown config) "/lib/lv2" ":"
     #$(pipewire-configuration-lsp-plugins config) "/lib/lv2"))

(define (pipewire-module-dir config)
  #~(string-append #$(pipewire-configuration-pipewire config)
                   "/lib/pipewire-0.3"))

(define (pipewire-asoundrc config)
  (match-record config <pipewire-configuration>
                (pipewire)
    (mixed-text-file
     "asoundrc"
     "<" pipewire "/share/alsa/alsa.conf.d/50-pipewire.conf>\n"
     "<" pipewire "/share/alsa/alsa.conf.d/99-pipewire-default.conf>\n"
     "pcm_type.pipewire {\n"
     "  lib \"" pipewire "/lib/alsa-lib/libasound_module_pcm_pipewire.so\"\n"
     "}\n"
     "ctl_type.pipewire {\n"
     "  lib \"" pipewire "/lib/alsa-lib/libasound_module_ctl_pipewire.so\"\n"
     "}\n")))

(define (pipewire-conf-dir config)
  (let ((asahi-audio (pipewire-configuration-asahi-audio config))
        (pipewire (pipewire-configuration-pipewire config)))
    (directory-union "pipewire" (list (file-append asahi-audio "/share/pipewire")
                                      (file-append pipewire "/share/pipewire")))))

(define (wireplumber-conf-dir config)
  (let ((asahi-audio (pipewire-configuration-asahi-audio config))
        (wireplumber (pipewire-configuration-wireplumber config)))
    (directory-union "wireplumber" (list (file-append asahi-audio "/share/wireplumber")
                                         (file-append wireplumber "/share/wireplumber")))))

(define pipewire-disable-pulseaudio-auto-start
  (plain-file "client.conf" "autospawn = no"))

(define (pipewire-etc-configuration config)
  (cons* `("alsa/asoundrc" ,(pipewire-asoundrc config))
         `("pipewire" ,(pipewire-conf-dir config))
         `("wireplumber" ,(wireplumber-conf-dir config))
         (if (pipewire-configuration-enable-pulseaudio? config)
             `(("pulse/client.conf"
                ,pipewire-disable-pulseaudio-auto-start))
             '())))

(define (pipewire-profile-entries config)
  (list (pipewire-configuration-lsp-plugins config)
        (pipewire-configuration-pipewire config)
        (pipewire-configuration-wireplumber config)))

(define (pipewire-environment-variables config)
  `(("ALSA_CONFIG_UCM2" . ,(alsa-config-ucm2 config))))

(define-public pipewire-service-type
  (service-type
   (name 'pipewire)
   (extensions
    (list (service-extension etc-service-type
                             pipewire-etc-configuration)
          (service-extension profile-service-type
                             pipewire-profile-entries)
          (service-extension session-environment-service-type
                             pipewire-environment-variables)))
   (description
    "Start essential PipeWire services.")
   (default-value (pipewire-configuration))))

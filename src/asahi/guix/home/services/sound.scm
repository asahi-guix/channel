(define-module (asahi guix home services sound)
  #:use-module (asahi guix packages audio)
  #:use-module (asahi guix packages crates-io)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu packages music)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (home-pipewire-configuration
            home-pipewire-service-type))


;;;
;;; PipeWire support.
;;;

(define-configuration/no-serialization home-pipewire-configuration
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

(define (home-pipewire-shepherd-service config)
  (shepherd-service
   (documentation "PipeWire media processing.")
   (provision '(pipewire))
   (requirement '(dbus))
   (start #~(make-forkexec-constructor
             (list #$(file-append
                      (home-pipewire-configuration-pipewire config)
                      "/bin/pipewire"))
             #:environment-variables
             (list "DISABLE_RTKIT=1"
                   (string-append
                    "ALSA_CONFIG_UCM2="
                    #$(home-pipewire-configuration-alsa-ucm-conf config)
                    "/share/alsa/ucm2")
                   (string-append
                    "LV2_PATH="
                    #$(home-pipewire-configuration-bankstown config)
                    "/lib/lv2"
                    ":"
                    #$(home-pipewire-configuration-lsp-plugins config)
                    "/lib/lv2")
                   (string-append
                    "PIPEWIRE_MODULE_DIR="
                    #$(home-pipewire-configuration-pipewire config)
                    "/lib/pipewire-0.3")
                   (string-append
                    "XDG_RUNTIME_DIR="
                    (or (getenv "XDG_RUNTIME_DIR")
                        (format #f "/run/user/~a" (getuid)))))))
   (stop #~(make-kill-destructor))))

(define (home-pipewire-pulseaudio-shepherd-service config)
  (shepherd-service
   (documentation "Drop-in PulseAudio replacement service for PipeWire.")
   (provision '(pipewire-pulseaudio))
   (requirement '(pipewire))
   (start #~(make-forkexec-constructor
             (list #$(file-append
                      (home-pipewire-configuration-pipewire config)
                      "/bin/pipewire-pulse"))
             #:environment-variables
             (list "DISABLE_RTKIT=1"
                   (string-append
                    "ALSA_CONFIG_UCM2="
                    #$(home-pipewire-configuration-alsa-ucm-conf config)
                    "/share/alsa/ucm2")
                   (string-append
                    "LV2_PATH="
                    #$(home-pipewire-configuration-bankstown config)
                    "/lib/lv2"
                    ":"
                    #$(home-pipewire-configuration-lsp-plugins config)
                    "/lib/lv2")
                   (string-append
                    "PIPEWIRE_MODULE_DIR="
                    #$(home-pipewire-configuration-pipewire config)
                    "/lib/pipewire-0.3")
                   (string-append
                    "XDG_RUNTIME_DIR="
                    (or (getenv "XDG_RUNTIME_DIR")
                        (format #f "/run/user/~a" (getuid)))))))
   (stop #~(make-kill-destructor))))

(define (home-wireplumber-shepherd-service config)
  (shepherd-service
   (documentation "WirePlumber session management for PipeWire.")
   (provision '(wireplumber))
   (requirement '(pipewire))
   (start #~(make-forkexec-constructor
             (list #$(file-append
                      (home-pipewire-configuration-wireplumber config)
                      "/bin/wireplumber"))
             #:environment-variables
             (list "DISABLE_RTKIT=1"
                   (string-append
                    "ALSA_CONFIG_UCM2="
                    #$(home-pipewire-configuration-alsa-ucm-conf config)
                    "/share/alsa/ucm2")
                   (string-append
                    "LV2_PATH="
                    #$(home-pipewire-configuration-bankstown config)
                    "/lib/lv2"
                    ":"
                    #$(home-pipewire-configuration-lsp-plugins config)
                    "/lib/lv2")
                   (string-append
                    "XDG_RUNTIME_DIR="
                    (or (getenv "XDG_RUNTIME_DIR")
                        (format #f "/run/user/~a" (getuid)))))))
   (stop #~(make-kill-destructor))))

(define (home-pipewire-shepherd-services config)
  (cons* (home-pipewire-shepherd-service config)
         (home-wireplumber-shepherd-service config)
         (if (home-pipewire-configuration-enable-pulseaudio? config)
             (list (home-pipewire-pulseaudio-shepherd-service config))
             '())))

(define (home-pipewire-asoundrc config)
  (match-record config <home-pipewire-configuration>
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

(define (combine-dirs name packages path)
  (directory-union name (map (lambda (package) (file-append package path)) packages)))

(define (home-pipewire-conf-dir config)
  (let ((asahi-audio (home-pipewire-configuration-asahi-audio config))
        (pipewire (home-pipewire-configuration-pipewire config)))
    (file-union
     "pipewire-config"
     `(("client.conf" ,(file-append pipewire "/share/pipewire/client.conf"))
       ("client.conf.avail" ,(file-append pipewire "/share/pipewire/client.conf.avail"))
       ("client-rt.conf" ,(file-append pipewire "/share/pipewire/client-rt.conf"))
       ("client-rt.conf.avail" ,(file-append pipewire "/share/pipewire/client-rt.conf.avail"))
       ("filter-chain" ,(file-append pipewire "/share/pipewire/filter-chain"))
       ("filter-chain.conf" ,(file-append pipewire "/share/pipewire/filter-chain.conf"))
       ("pipewire.conf" ,(file-append pipewire "/share/pipewire/pipewire.conf"))
       ("pipewire.conf.avail" ,(file-append pipewire "/share/pipewire/pipewire.conf.avail"))
       ("pipewire.conf.d" ,(combine-dirs "pipewire.conf.d" (list asahi-audio) "/share/pipewire/pipewire.conf.d"))
       ("pipewire-pulse.conf" ,(file-append pipewire "/share/pipewire/pipewire-pulse.conf"))
       ("pipewire-pulse.conf.avail" ,(file-append pipewire "/share/pipewire/pipewire-pulse.conf.avail"))
       ("pipewire-pulse.conf.d" ,(combine-dirs "pipewire-pulse.conf.d" (list asahi-audio) "/share/pipewire/pipewire-pulse.conf.d"))))))

(define (home-wireplumber-conf-dir config)
  (let ((asahi-audio (home-pipewire-configuration-asahi-audio config))
        (wireplumber (home-pipewire-configuration-wireplumber config)))
    (file-union
     "wireplumber-config"
     `(("bluetooth.conf" ,(file-append wireplumber "/share/wireplumber/bluetooth.conf"))
       ("bluetooth.lua.d" ,(file-append wireplumber "/share/wireplumber/bluetooth.lua.d"))
       ("common" ,(file-append wireplumber "/share/wireplumber/common"))
       ("main.conf" ,(file-append wireplumber "/share/wireplumber/main.conf"))
       ("main.lua.d" ,(combine-dirs "main.lua.d" (list asahi-audio wireplumber) "/share/wireplumber/main.lua.d"))
       ("policy.conf" ,(file-append wireplumber "/share/wireplumber/policy.conf"))
       ("policy.lua.d" ,(combine-dirs "policy.lua.d" (list asahi-audio wireplumber) "/share/wireplumber/policy.lua.d"))
       ("scripts" ,(combine-dirs "scripts" (list asahi-audio wireplumber) "/share/wireplumber/scripts"))
       ("wireplumber.conf" ,(file-append wireplumber "/share/wireplumber/wireplumber.conf"))
       ("wireplumber.conf.d" ,(combine-dirs "wireplumber.conf.d" (list asahi-audio) "/share/wireplumber/wireplumber.conf.d"))))))

(define home-pipewire-disable-pulseaudio-auto-start
  (plain-file "client.conf" "autospawn = no"))

(define (home-pipewire-xdg-configuration config)
  (cons* `("alsa/asoundrc" ,(home-pipewire-asoundrc config))
         `("pipewire" ,(home-pipewire-conf-dir config))
         `("wireplumber" ,(home-wireplumber-conf-dir config))
         (if (home-pipewire-configuration-enable-pulseaudio? config)
             `(("pulse/client.conf"
                ,home-pipewire-disable-pulseaudio-auto-start))
             '())))

(define (home-pipewire-profile-entries config)
  (list (home-pipewire-configuration-lsp-plugins config)
        (home-pipewire-configuration-pipewire config)
        (home-pipewire-configuration-wireplumber config)))

(define home-pipewire-service-type
  (service-type
   (name 'pipewire)
   (extensions
    (list (service-extension home-profile-service-type
                             home-pipewire-profile-entries)
          (service-extension home-shepherd-service-type
                             home-pipewire-shepherd-services)
          (service-extension home-xdg-configuration-files-service-type
                             home-pipewire-xdg-configuration)))
   (description
    "Start essential PipeWire services.")
   (default-value (home-pipewire-configuration))))

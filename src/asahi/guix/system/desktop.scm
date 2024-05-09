(define-module (asahi guix system desktop)
  #:use-module ((gnu services sound) #:prefix sound:)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix services console-font)
  #:use-module (asahi guix services firmware)
  #:use-module (asahi guix services sound)
  #:use-module (asahi guix services speakersafetyd)
  #:use-module (asahi guix substitutes)
  #:use-module (asahi guix system base)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu services sddm)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system)
  #:use-module (guix packages)
  #:export (asahi-desktop-operating-system
            asahi-gnome-desktop-operating-system))

(define asahi-desktop-operating-system
  (let ((base asahi-edge-operating-system))
    (operating-system
      (inherit base)
      (services (modify-services (cons* fontconfig-file-system-service
                                        polkit-wheel-service
                                        (service accountsservice-service-type)
                                        (service avahi-service-type)
                                        (service colord-service-type)
                                        (service cups-pk-helper-service-type)
                                        (service dbus-root-service-type)
                                        (service elogind-service-type)
                                        (service geoclue-service-type)
                                        (service modem-manager-service-type)
                                        (service ntp-service-type)
                                        (service polkit-service-type)
                                        (service speakersafetyd-service-type)
                                        (service udisks-service-type)
                                        (service upower-service-type)
                                        (service usb-modeswitch-service-type)
                                        (simple-service 'mtp udev-service-type (list libmtp))
                                        (operating-system-user-services base))))
      (packages (cons* emacs kitty sway (operating-system-packages base))))))

;; Gnome Desktop

(define %xorg-libinput-config "
Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"
  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection

Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define %xorg-modeset-config "
Section \"OutputClass\"
    Identifier \"appledrm\"
    MatchDriver \"apple\"
    Driver \"modesetting\"
    Option \"PrimaryGPU\" \"true\"
EndSection
")

(define %gnome-desktop-configuration
  (gnome-desktop-configuration
   (shell (map cadr (modify-inputs (package-propagated-inputs gnome-meta-core-shell)
                      ;; These packages can't be built.
                      (delete "orca" "rygel"))))))

(define %gnome-desktop-services
  (modify-services (cons* (service asahi-firmware-service-type)
                          (service gdm-service-type)
                          (service gnome-desktop-service-type %gnome-desktop-configuration)
                          (service kernel-module-loader-service-type '("asahi" "appledrm"))
                          (service pipewire-service-type)
                          (service speakersafetyd-service-type)
                          %desktop-services)
    (delete sddm-service-type)
    (delete sound:alsa-service-type)
    (delete sound:pulseaudio-service-type)
    (console-font-service-type config => (console-font-terminus config))
    (gdm-service-type config =>
                      (gdm-configuration
                       (inherit config)
                       (xorg-configuration
                        (xorg-configuration
                         (extra-config (list %xorg-libinput-config
                                             %xorg-modeset-config))))))
    (guix-service-type config => (append-substitutes config))))

(define asahi-gnome-desktop-operating-system
  (let ((base asahi-edge-operating-system))
    (operating-system
      (inherit base)
      (services %gnome-desktop-services)
      (packages (cons* emacs (operating-system-packages base))))))

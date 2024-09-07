(define-module (asahi guix systems desktop)
  #:use-module ((gnu services sound) #:prefix sound:)
  #:use-module (asahi guix home services sound)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages gl)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix packages pulseaudio)
  #:use-module (asahi guix packages wm)
  #:use-module (asahi guix packages xorg)
  #:use-module (asahi guix services console-font)
  #:use-module (asahi guix services firmware)
  #:use-module (asahi guix services sound)
  #:use-module (asahi guix services speakersafetyd)
  #:use-module (asahi guix substitutes)
  #:use-module (asahi guix systems base)
  #:use-module (gnu artwork)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services guix)
  #:use-module (gnu services linux)
  #:use-module (gnu services networking)
  #:use-module (gnu services sddm)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define %asahi-desktop-background
  (file-append %artwork-repository "/backgrounds/guix-silver-checkered-16-9.svg"))

(define %asahi-desktop-home-services
  (list (service home-dbus-service-type)
        (service home-pipewire-service-type)))

(define %asahi-desktop-home-environment
  (home-environment (services %asahi-desktop-home-services)))

(define %asahi-desktop-home-service
  (service guix-home-service-type `(("guest" ,%asahi-desktop-home-environment))))

(define %asahi-desktop-kernel-modules
  (service kernel-module-loader-service-type '("asahi" "appledrm")))

(define %asahi-desktop-packages
  (cons* asahi-alsa-utils
         asahi-mesa-utils
         asahi-pulseaudio
         brightnessctl
         emacs
         kitty
         librewolf
         network-manager
         (remove (lambda (package)
                   (equal? "network-manager" (package-name package)))
                 (map replace-mesa (operating-system-packages asahi-edge-os)))))

(define %asahi-desktop-packages
  (cons* asahi-alsa-utils
         asahi-mesa-utils
         asahi-pulseaudio
         brightnessctl
         ;; TODO: Why does network-manager appear twice?
         network-manager
         (remove (lambda (package)
                   (equal? "network-manager" (package-name package)))
                 (map replace-mesa
                      (cons* emacs
                             kitty
                             librewolf
                             (operating-system-packages asahi-edge-os))))))

(define %asahi-desktop-xorg-touchpads "
  Section \"InputClass\"
    Driver \"libinput\"
    Identifier \"Touchpads\"
    MatchDevicePath \"/dev/input/event*\"
    MatchIsTouchpad \"on\"
    Option \"DisableWhileTyping\" \"on\"
    Option \"MiddleEmulation\" \"on\"
    Option \"ScrollMethod\" \"twofinger\"
    Option \"TappingDrag\" \"on\"
    Option \"Tapping\" \"on\"
  EndSection\n")

(define %asahi-desktop-xorg-keyboards "
  Section \"InputClass\"
    Driver \"libinput\"
    Identifier \"Keyboards\"
    MatchDevicePath \"/dev/input/event*\"
    MatchIsKeyboard \"on\"
  EndSection\n")

(define %asahi-desktop-xorg-modesetting "
  Section \"OutputClass\"
    Driver \"modesetting\"
    Identifier \"appledrm\"
    MatchDriver \"apple\"
    Option \"PrimaryGPU\" \"true\"
  EndSection\n")

(define %asahi-xorg-configuration
  (xorg-configuration
   (server asahi-xorg-server)
   (extra-config (list %asahi-desktop-xorg-keyboards
                       %asahi-desktop-xorg-touchpads
                       %asahi-desktop-xorg-modesetting))))

(define %asahi-gdm-service
  (service gdm-service-type
           (gdm-configuration
            (xorg-configuration %asahi-xorg-configuration))))

(define %asahi-sddm-service
  (service sddm-service-type
           (sddm-configuration
            (xorg-configuration %asahi-xorg-configuration))))

(define %asahi-desktop-services
  (modify-services (cons* %asahi-desktop-kernel-modules
                          (service alsa-service-type)
                          (service asahi-firmware-service-type)
                          (service openssh-service-type)
                          (service speakersafetyd-service-type)
                          (remove (lambda (service)
                                    (or (eq? (service-kind service) gdm-service-type)
                                        (eq? (service-kind service) sddm-service-type)))
                                  %desktop-services))
    (delete sound:alsa-service-type)
    (delete sound:pulseaudio-service-type)))

;; Gnome

(define %asahi-gnome-shell
  (map cadr (modify-inputs (package-propagated-inputs gnome-meta-core-shell)
              (delete "orca" "rygel")))) ;; These packages can't be built.

(define-public asahi-gnome-os
  (operating-system
    (inherit asahi-edge-os)
    (services (cons* (service gnome-desktop-service-type
                              (gnome-desktop-configuration
                               (shell %asahi-gnome-shell)))
                     %asahi-desktop-home-service
                     %asahi-gdm-service
                     %asahi-desktop-services))
    (packages %asahi-desktop-packages)))

;; Plasma

(define-public asahi-plasma-os
  (operating-system
    (inherit asahi-edge-os)
    (services (cons* (service plasma-desktop-service-type
                              (plasma-desktop-configuration
                               (plasma-package (replace-mesa plasma))))
                     %asahi-desktop-home-service
                     %asahi-sddm-service
                     %asahi-desktop-services))
    (packages %asahi-desktop-packages)))

;; Sway

(define %asahi-desktop-variables
  '(("CLUTTER_BACKEND" . "wayland") ; GTK
    ("QT_QPA_PLATFORM" . "wayland") ; Qt
    ("MOZ_ENABLE_WAYLAND" . "1") ; IceCat, et.al.
    ;; These are normally provided by login managers(?).
    ("XDG_SESSION_TYPE" . "wayland")
    ("XDG_SESSION_DESKTOP" . "sway")
    ("XDG_CURRENT_DESKTOP" . "sway")))

(define %asahi-sway-home-service
  (service guix-home-service-type
           `(("guest" ,(home-environment
                        (services %asahi-desktop-home-services))))))

(define %asahi-sway-packages
  (append (map replace-mesa (list emacs-pgtk dmenu foot sway wofi))
          (remove (lambda (package)
                    (eq? "emacs" (package-name package)))
                  %asahi-desktop-packages)))

(define-public asahi-sway-os
  (operating-system
    (inherit asahi-edge-os)
    (services (modify-services (cons* %asahi-sway-home-service
                                      %asahi-sddm-service
                                      %asahi-desktop-services)))
    (packages %asahi-sway-packages)))

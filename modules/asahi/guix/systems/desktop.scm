(define-module (asahi guix systems desktop)
  #:use-module ((gnu services sound) #:prefix sound:)
  #:use-module (asahi guix home services sound)
  #:use-module (asahi guix packages gl)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix packages pulseaudio)
  #:use-module (asahi guix packages wm)
  #:use-module (asahi guix packages xorg)
  #:use-module (asahi guix services firmware)
  #:use-module (asahi guix services sound)
  #:use-module (asahi guix systems base)
  #:use-module (asahi guix transformations)
  #:use-module (gnu artwork)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services guix)
  #:use-module (gnu services linux)
  #:use-module (gnu services sddm)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (%asahi-gdm-service
            %asahi-sddm-service
            %asahi-desktop-home-service
            %asahi-desktop-home-services
            %asahi-desktop-packages
            %asahi-desktop-services))

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
         ;; TODO: Why does network-manager appear twice?
         network-manager
         (remove (lambda (package)
                   (equal? "network-manager" (package-name package)))
                 (map replace-asahi
                      (cons* emacs
                             kitty
                             librewolf
                             (operating-system-packages asahi-base-os))))))

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
                          (service sound:speakersafetyd-service-type)
                          (remove (lambda (service)
                                    (or (eq? (service-kind service) gdm-service-type)
                                        (eq? (service-kind service) sddm-service-type)))
                                  %desktop-services))
    (delete sound:alsa-service-type)
    (delete sound:pulseaudio-service-type)))

(define-module (asahi guix)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (guix git-download)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu services)
  #:use-module (gnu system install)
  #:use-module (gnu system)
  #:use-module (guix)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

(define-public asahi-linux
  (package
    (inherit linux)
    (name "asahi-linux")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/linux.git")
             (commit "6ecde4985ffc8490eb3dbc9303d338b8872e3991")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05r2i3dnwa9v35x93p6r6ixnf456annfx498jgmviwl53jkxi1qc"))))
    (home-page "https://asahilinux.org")
    (synopsis "Linux on Apple Silicon")
    (description "Asahi Linux is a project and community with the goal of porting Linux
to Apple Silicon Macs, starting with the 2020 M1 Mac Mini, MacBook
Air, and MacBook Pro.")))

(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel asahi-linux)
    ;; (kernel linux)
    (firmware (list linux-firmware))

    ;; Add the 'net.ifnames' argument to prevent network interfaces
    ;; from having really long names.  This can cause an issue with
    ;; wpa_supplicant when you try to connect to a wifi network.
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.ifnames=0"))

    (services
     (cons*
      ;; Include the channel file so that it can be used during installation
      (simple-service 'channel-file etc-service-type
                      (list `("channels.scm" ,(local-file "channels.scm"))))
      (operating-system-user-services installation-os)))

    ;; Add some extra packages useful for the installation process
    (packages
     (append (list git curl stow vim emacs-no-x-toolkit)
             (operating-system-packages installation-os)))))

installation-os-nonfree

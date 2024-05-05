(define-module (asahi guix machine server)
  #:use-module (asahi guix system server)
  #:use-module (gnu machine ssh)
  #:use-module (gnu machine)
  #:use-module (gnu)
  #:export (asahi-guix-server-machine))

(define host-key
  (string-append "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAhdngke8Fhf7ruQFij8riXXiFIfkPi/XiX262TzTLYM root@(none)"))

(define asahi-guix-server-machine
  (machine
   (operating-system asahi-guix-server-system)
   (environment managed-host-environment-type)
   (configuration (machine-ssh-configuration
                   (allow-downgrades? #t)
                   (build-locally? #f)
                   (host-key host-key)
                   (host-name "www.asahi-guix.org")
                   (port 22)
                   (system "aarch64-linux")
                   (user "root")))))

(list asahi-guix-server-machine)

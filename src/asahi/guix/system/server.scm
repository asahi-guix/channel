(define-module (asahi guix system server)
  #:use-module (asahi guix packages ci)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (gnu services admin)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services certbot)
  #:use-module (gnu services cuirass)
  #:use-module (gnu services databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services security)
  #:use-module (gnu services sound)
  #:use-module (gnu services ssh)
  #:use-module (gnu services ssh)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services web)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system nss)
  #:use-module (gnu system uuid)
  #:use-module (gnu system)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:export (asahi-guix-server-system))

(define %keyboard-layout
  (keyboard-layout "us" #:options '("caps:ctrl_modifier")))

(define %bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (keyboard-layout %keyboard-layout)
   (targets (list "/boot/efi"))))

(define %file-systems
  (cons* (file-system
           (mount-point "/")
           (device "/dev/sda1")
           (type "ext4")
           (needed-for-boot? #t))
         (file-system
           (mount-point "/boot/efi")
           (device "/dev/sda15")
           (type "vfat"))
         %base-file-systems))

(define %initrd-modules
  (cons* "sd_mod" "virtio_scsi" %base-initrd-modules))

(define %packages
  (append (map specification->package
               '("e2fsprogs"
                 "git"
                 "htop"
                 "net-tools"
                 "network-manager"
                 "nss-certs"
                 "screen"))
          %base-packages))

(define %users
  (list (user-account
         (name "roman")
         (comment "Roman")
         (group "users")
         (home-directory "/home/roman")
         (supplementary-groups '("audio" "netdev" "video" "wheel")))))

(define %avahi-service
  (service avahi-service-type))

(define %fail2ban-service
  (service fail2ban-service-type
           (fail2ban-configuration
            (extra-jails
             (list
              (fail2ban-jail-configuration
               (name "sshd")
               (enabled? #t)))))))

(define %firewall-service
  (service iptables-service-type
           (iptables-configuration
            (ipv4-rules (plain-file "iptables.rules" "*filter
:INPUT ACCEPT
:FORWARD ACCEPT
:OUTPUT ACCEPT
-A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
-A INPUT -p tcp --dport 22 -j ACCEPT
-A INPUT -p tcp --dport 443 -j ACCEPT
-A FORWARD -p tcp --dport 443 -j ACCEPT
-A PREROUTING -t nat -p tcp --dport 443 -j DNAT --to-destination 127.0.0.1
-A POSTROUTING -t nat -p tcp -d 127.0.0.1 -j MASQUERADE
-A INPUT -j REJECT --reject-with icmp-port-unreachable
COMMIT
"))
            (ipv6-rules (plain-file "ip6tables.rules" "*filter
:INPUT ACCEPT
:FORWARD ACCEPT
:OUTPUT ACCEPT
-A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
-A INPUT -p tcp --dport 22 -j ACCEPT
-A INPUT -p tcp --dport 443 -j ACCEPT
-A FORWARD -p tcp --dport 443 -j ACCEPT
-A PREROUTING -t nat -p tcp --dport 443 -j DNAT --to-destination 127.0.0.1
-A POSTROUTING -t nat -p tcp -d 127.0.0.1 -j MASQUERADE
-A INPUT -j REJECT --reject-with icmp6-port-unreachable
COMMIT
")))))

(define %ntp-service
  (service ntp-service-type))

(define %openssh-service
  (service openssh-service-type
           (openssh-configuration
            (authorized-keys
             `(("root" ,(local-file "../files/ssh/authorized-keys/root.pub"))
               ("root" ,(local-file "../files/ssh/authorized-keys/roman.pub"))
               ("roman" ,(local-file "../files/ssh/authorized-keys/roman.pub"))))
            (openssh openssh-sans-x)
            (permit-root-login 'prohibit-password)
            (port-number 22))))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define %certbot-service
  (service certbot-service-type
           (certbot-configuration
            (email "roman@asahi-guix.org")
            (certificates
             (list
              (certificate-configuration
               (domains '("ci.asahi-guix.org"))
               (deploy-hook %nginx-deploy-hook))
              (certificate-configuration
               (domains '("substitutes.asahi-guix.org"))
               (deploy-hook %nginx-deploy-hook))
              (certificate-configuration
               (domains '("www.asahi-guix.org"))
               (deploy-hook %nginx-deploy-hook)))))))

(define %cuirass-service
  (service cuirass-service-type
           (cuirass-configuration
            (cuirass cuirass-disable-jit)
            (specifications
             #~(list (specification
                      (name "guix")
                      (build '(packages "jemalloc" "icecat" "rust"))
                      (channels
                       (list (channel
                              (name 'guix)
                              (branch "main")
                              (url "https://github.com/asahi-guix/guix.git")
                              (introduction
                               (make-channel-introduction
                                "047f4ab1dbd2dbd63f2f1adc4edf9b73ea71a7dc"
                                (openpgp-fingerprint
                                 "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))))
                      (systems '("aarch64-linux")))
                     (specification
                      (name "asahi")
                      (build '(channels asahi))
                      (channels
                       (list (channel
                              (name 'guix)
                              (branch "main")
                              (url "https://github.com/asahi-guix/guix.git")
                              (introduction
                               (make-channel-introduction
                                "047f4ab1dbd2dbd63f2f1adc4edf9b73ea71a7dc"
                                (openpgp-fingerprint
                                 "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))
                             (channel
                              (name 'asahi)
                              (branch "main")
                              (url "https://github.com/asahi-guix/channel.git")
                              (introduction
                               (make-channel-introduction
                                "3eeb493b037bea44f225c4314c5556aa25aff36c"
                                (openpgp-fingerprint
                                 "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))))
                      (systems '("aarch64-linux")))))
            (use-substitutes? #t)
            (remote-server (cuirass-remote-server-configuration)))))

(define %cuirass-remote-worker-service
  (service cuirass-remote-worker-service-type
           (cuirass-remote-worker-configuration
            (systems '("aarch64-linux"))
            (workers 1))))

(define (certbot-ssl-certificate domain)
  (format #f "/etc/letsencrypt/live/~a/fullchain.pem" domain))

(define (certbot-ssl-certificate-key domain)
  (format #f "/etc/letsencrypt/live/~a/privkey.pem" domain))

(define %http-service
  (service
   nginx-service-type
   (nginx-configuration
    (server-blocks
     (list
      (nginx-server-configuration
       (server-name '("www.asahi-guix.org"))
       (listen '("443 ssl" "[::]:443 ssl"))
       (ssl-certificate (certbot-ssl-certificate "www.asahi-guix.org"))
       (ssl-certificate-key (certbot-ssl-certificate-key "www.asahi-guix.org"))
       (locations
        (list
         (nginx-location-configuration
          (uri "/")
          (body '("return 404;"))))))
      (nginx-server-configuration
       (server-name '("ci.asahi-guix.org"))
       (listen '("443 ssl" "[::]:443 ssl"))
       (ssl-certificate (certbot-ssl-certificate "ci.asahi-guix.org"))
       (ssl-certificate-key (certbot-ssl-certificate-key "ci.asahi-guix.org"))
       (locations
        (list
         (nginx-location-configuration
          (uri "~ ^/admin")
          (body (list "if ($ssl_client_verify != SUCCESS) { return 403; } proxy_pass http://cuirass;")))
         (nginx-location-configuration
          (uri "/")
          (body '("proxy_pass http://cuirass;"))))))
      (nginx-server-configuration
       (server-name '("substitutes.asahi-guix.org"))
       (listen '("443 ssl" "[::]:443 ssl"))
       (ssl-certificate (certbot-ssl-certificate "substitutes.asahi-guix.org"))
       (ssl-certificate-key (certbot-ssl-certificate-key "substitutes.asahi-guix.org"))
       (locations
        (list
         (nginx-location-configuration
          (uri "/")
          (body '("proxy_pass http://guix-publish;"))))))))
    (upstream-blocks
     (list (nginx-upstream-configuration
            (name "cuirass")
            (servers (list "127.0.0.1:8081")))
           (nginx-upstream-configuration
            (name "guix-publish")
            (servers (list "127.0.0.1:8082"))))))))

(define %http-service-bootstrap
  (service
   nginx-service-type
   (nginx-configuration
    (server-blocks
     (list
      (nginx-server-configuration
       (locations
        (list
         (nginx-location-configuration
          (uri "/")
          (body '("return 404;")))))))))))

(define %unattended-upgrade-service
  (service unattended-upgrade-service-type
           (unattended-upgrade-configuration
            (channels #~(list (channel
                               (name 'guix)
                               (branch "main")
                               (url "https://github.com/asahi-guix/guix.git")
                               (introduction
                                (make-channel-introduction
                                 "047f4ab1dbd2dbd63f2f1adc4edf9b73ea71a7dc"
                                 (openpgp-fingerprint
                                  "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))
                              (channel
                               (name 'asahi)
                               (branch "main")
                               (url "https://github.com/asahi-guix/channel.git")
                               (introduction
                                (make-channel-introduction
                                 "7677591b60ae62f76d8fcee392f0b249414442f6"
                                 (openpgp-fingerprint
                                  "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))))
            (schedule "0 4 * * *")
            (services-to-restart
             '(avahi-daemon
               console-font-tty1
               console-font-tty2
               console-font-tty3
               console-font-tty4
               console-font-tty5
               console-font-tty6
               cuirass
               cuirass-remote-server
               cuirass-remote-worker
               cuirass-web
               dbus-system
               fail2ban
               guix-daemon
               guix-publish
               mcron
               nginx
               nscd
               pam
               postgres
               qemu-binfmt
               ssh-daemon
               syslogd
               term-console
               term-tty1
               term-tty2
               term-tty3
               term-tty4
               term-tty5
               term-tty6
               virtual-terminal)))))

(define %guix-publish-service
  (service guix-publish-service-type
           (guix-publish-configuration
            (compression '(("zstd" 3)))
            (port 8082))))

(define %postgresql-service
  (service postgresql-service-type
           (postgresql-configuration
            (postgresql postgresql-15))))

(define %qemu-service-x86-64
  (service qemu-binfmt-service-type
           (qemu-binfmt-configuration
            ;; Note: Don't emulate the arch the system is running under.
            (platforms (lookup-qemu-platforms "x86_64")))))

(define %services
  (cons* %avahi-service
         %certbot-service
         %cuirass-remote-worker-service
         %cuirass-service
         %fail2ban-service
         %firewall-service
         %guix-publish-service
         ;; %http-service-bootstrap
         %http-service
         %ntp-service
         %openssh-service
         %postgresql-service
         %qemu-service-x86-64
         %unattended-upgrade-service
         (service dhcp-client-service-type)
         %base-services))

(define %swap-devices
  (list (swap-space (target "/swapfile"))))

(define asahi-guix-server-system
  (operating-system
    (host-name "asahi-guix")
    (timezone "Etc/UTC")
    (locale "en_US.utf8")
    (kernel linux-libre)
    (bootloader %bootloader)
    (initrd-modules %initrd-modules)
    (file-systems %file-systems)
    (packages %packages)
    (services %services)
    (users %users)
    (swap-devices %swap-devices)))

asahi-guix-server-system

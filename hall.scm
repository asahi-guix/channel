(hall-description
 (name "asahi-guix")
 (prefix "guile")
 (version "0.1")
 (author "r0man")
 (copyright (2023))
 (synopsis "Asahi Guix")
 (description "Asahi Linux on GNU Guix")
 (home-page "https://github.com/r0man/asahi-guix")
 (license gpl3+)
 (dependencies `())
 (skip ())
 (files (libraries
         ((directory
           "asahi"
           ((directory
             "guix"
             ((directory "bootloader" ((scheme-file "m1n1")))
              (directory
               "packages"
               ((scheme-file "linux")
                (scheme-file "crates-io")
                (scheme-file "gl")
                (scheme-file "guile-xyz")
                (unknown-type "defconfig.main")
                (unknown-type "defconfig.edge")
                (scheme-file "audio")
                (scheme-file "xdisorg")
                (scheme-file "firmware")
                (scheme-file "rust")
                (scheme-file "bootloader")
                (scheme-file "jemalloc")
                (scheme-file "misc")))
              (directory
               "build"
               ((directory "bootloader" ((scheme-file "m1n1")))
                (scheme-file "block-devices")
                (scheme-file "firmware")))
              (directory
               "system"
               ((scheme-file "desktop")
                (scheme-file "base")
                (scheme-file "install")))
              (directory
               "services"
               ((scheme-file "channels")
                (scheme-file "firmware")
                (scheme-file "udev")
                (scheme-file "console-font")))
              (scheme-file "channels")
              (unknown-type "substitutes.asahi-guix.org.pub")
              (scheme-file "initrd")
              (scheme-file "udev")
              (scheme-file "transformations")
              (scheme-file "substitutes")))))))
        (tests ((directory
                 "tests"
                 ((directory
                   "asahi"
                   ((directory
                     "guix"
                     ((directory
                       "build"
                       ((scheme-file "block-devices")))))))))))
        (programs ((directory "scripts" ())))
        (documentation
         ((directory "doc" ((texi-file "ashai-guix")))
          (org-file "NOTES")
          (org-file "README")
          (symlink "README" "README.org")
          (text-file "COPYING")
          (text-file "HACKING")))
        (infrastructure
         ((scheme-file "hall")
          (text-file ".guix-authorizations")
          (text-file ".guix-channel")
          (text-file ".gitignore")
          (scheme-file "guix")
          (directory
           "share"
           ((directory
             "systems"
             ((unknown-type "asahi-guix.tmpl")
              (unknown-type "asahi-guix-edge.tmpl")))))
          (directory
           ".github"
           ((directory
             "workflows"
             ((unknown-type "aarch64-linux-gnu.yml")
              (unknown-type "x86_64-linux-gnu.yml")))
            (unknown-type "FUNDING.yml")))))))

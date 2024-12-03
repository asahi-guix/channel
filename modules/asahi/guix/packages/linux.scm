(define-module (asahi guix packages linux)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix build utils)
  #:use-module (asahi guix packages rust)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

;; Based on https://github.com/AsahiLinux/docs/wiki/Kernel-config-notes-for-distros

(define %config
  '(("CONFIG_APPLE_MFI_FASTCHARGE" . m)
    ("CONFIG_AQTION" . m)
    ("CONFIG_ARM64_16K_PAGES" . #t)
    ("CONFIG_BCACHE" . m)
    ("CONFIG_BLK_DEV_DM" . #t)
    ("CONFIG_BLK_DEV_MD" . #t)
    ("CONFIG_BRCMFMAC" . m)
    ("CONFIG_BRCMFMAC_PCIE" . #t)
    ("CONFIG_BRCMFMAC_PROTO_BCDC" . #t)
    ("CONFIG_BRCMFMAC_PROTO_MSGBUF" . #t)
    ("CONFIG_BRCMFMAC_USB" . #t)
    ("CONFIG_BT" . m)
    ("CONFIG_BT_BNEP" . m)
    ("CONFIG_BT_BREDR" . #t)
    ("CONFIG_BT_HCIUART_BCM" . #t)
    ("CONFIG_BT_HIDP" . m)
    ("CONFIG_BT_LE" . #t)
    ("CONFIG_BT_RFCOMM" . m)
    ("CONFIG_CFG80211" . m)
    ("CONFIG_CPU_FREQ_DEFAULT_GOV_SCHEDUTIL" . #t)
    ("CONFIG_CRYPTO_SERPENT" . m)
    ("CONFIG_CRYPTO_USER_API_AEAD" . #t)
    ("CONFIG_CRYPTO_USER_API_HASH" . #t)
    ("CONFIG_CRYPTO_USER_API_RNG" . #t)
    ("CONFIG_CRYPTO_USER_API_SKCIPHER" . #t)
    ("CONFIG_CRYPTO_WP512" . m)
    ("CONFIG_DM_CACHE" . m)
    ("CONFIG_DM_CRYPT" . m)
    ("CONFIG_DM_CRYPT" . m)
    ("CONFIG_DM_DEBUG" . #t)
    ("CONFIG_DM_DELAY" . m)
    ("CONFIG_DM_DUST" . m)
    ("CONFIG_DM_FLAKEY" . m)
    ("CONFIG_DM_INIT" . #t)
    ("CONFIG_DM_INTEGRITY" . m)
    ("CONFIG_DM_LOG_USERSPACE" . m)
    ("CONFIG_DM_LOG_WRITES" . m)
    ("CONFIG_DM_MIRROR" . #t)
    ("CONFIG_DM_MULTIPATH" . m)
    ("CONFIG_DM_MULTIPATH_QL" . m)
    ("CONFIG_DM_MULTIPATH_ST" . m)
    ("CONFIG_DM_RAID" . m)
    ("CONFIG_DM_SNAPSHOT" . #t)
    ("CONFIG_DM_SWITCH" . m)
    ("CONFIG_DM_THIN_PROVISIONING" . m)
    ("CONFIG_DM_UEVENT" . #t)
    ("CONFIG_DM_VERITY" . m)
    ("CONFIG_DM_VERITY_FEC" . #t)
    ("CONFIG_DM_WRITECACHE" . m)
    ("CONFIG_DM_ZERO" . #t)
    ("CONFIG_DM_ZONED" . m)
    ("CONFIG_DRM_ASAHI_DEBUG_ALLOCATOR" . #f)
    ("CONFIG_DRM_CIRRUS_QEMU" . m)
    ("CONFIG_DRM_FBDEV_EMULATION" . #t)
    ("CONFIG_DRM_GEM_SHMEM_HELPER" . #t)
    ("CONFIG_DRM_SCHED" . #t)
    ("CONFIG_DRM_VGEM" . #f)
    ("CONFIG_ENERGY_MODEL" . #t)
    ("CONFIG_GCC_PLUGINS" . #f)
    ("CONFIG_HIBERNATION" . #f)
    ("CONFIG_HID_APPLE" . m)
    ("CONFIG_HID_BATTERY_STRENGTH" . #t)
    ("CONFIG_HID_MAGICMOUSE" . m)
    ("CONFIG_INPUT_LEDS" . #t)
    ("CONFIG_LEDS_PWM" . #t)
    ("CONFIG_MD" . #t)
    ("CONFIG_MD_CLUSTER" . m)
    ("CONFIG_MMC_CQHCI" . m)
    ("CONFIG_MMC_SDHCI" . m)
    ("CONFIG_MMC_SDHCI_PCI" . m)
    ("CONFIG_MOUSE_APPLETOUCH" . m)
    ("CONFIG_NET_VENDOR_AQUANTIA" . #t)
    ("CONFIG_NET_VENDOR_BROADCOM" . #t)
    ("CONFIG_NLS_ISO8859_1" . m)
    ("CONFIG_POWER_RESET_GPIO" . #t)
    ("CONFIG_REGULATOR_FIXED_VOLTAGE" . #t)
    ("CONFIG_SERIAL_SAMSUNG" . #t)
    ("CONFIG_SERIAL_SAMSUNG_CONSOLE" . #t)
    ("CONFIG_SND_SOC_CS42L83" . m)
    ("CONFIG_SND_SOC_TAS2764" . m)
    ("CONFIG_SND_SOC_TAS2770" . m)
    ("CONFIG_SUSPEND" . #t)
    ("CONFIG_TIGON3" . m)
    ("CONFIG_TYPEC_TPS6598X" . m)
    ("CONFIG_UCLAMP_TASK" . #t)
    ("CONFIG_UCLAMP_TASK_GROUP" . #t)
    ("CONFIG_USB_DWC3" . m)
    ("CONFIG_USB_DWC3_DUAL_ROLE" . #t)
    ("CONFIG_USB_HID" . m)
    ("CONFIG_USB_STORAGE" . m)
    ("CONFIG_USB_UAS" . m)
    ("CONFIG_USB_XHCI_HCD" . m)
    ("CONFIG_USB_XHCI_PCI_ASMEDIA" . #t)
    ("CONFIG_USB_XHCI_PCI_RENESAS" . #t)
    ("CONFIG_USB_XHCI_PLATFORM" . m)
    ("CONFIG_WATCHDOG_HANDLE_BOOT_ENABLED" . #t)
    ("CONFIG_WLAN_VENDOR_BROADCOM" . #t)))

(define %base-config
  (append '(("CONFIG_APPLE_ADMAC" . #t)
            ("CONFIG_APPLE_AIC" . #t)
            ("CONFIG_APPLE_DART" . m)
            ("CONFIG_APPLE_DOCKCHANNEL" . m)
            ("CONFIG_APPLE_M1_CPU_PMU" . #t)
            ("CONFIG_APPLE_MAILBOX" . #t)
            ("CONFIG_APPLE_PLATFORMS" . #t)
            ("CONFIG_APPLE_PMGR_MISC" . #t)
            ("CONFIG_APPLE_PMGR_PWRSTATE" . #t)
            ("CONFIG_APPLE_RTKIT" . m)
            ("CONFIG_APPLE_RTKIT_HELPER" . m)
            ("CONFIG_APPLE_SART" . #t)
            ("CONFIG_APPLE_SIO" . m)
            ("CONFIG_APPLE_SMC" . m)
            ("CONFIG_APPLE_SMC_RTKIT" . m)
            ("CONFIG_APPLE_WATCHDOG" . #t)
            ("CONFIG_ARCH_APPLE" . #t)
            ("CONFIG_ARM64_MEMORY_MODEL_CONTROL" . #t)
            ("CONFIG_ARM_APPLE_CPUIDLE" . #t)
            ("CONFIG_ARM_APPLE_SOC_CPUFREQ" . #t)
            ("CONFIG_BT_HCIBCM4377" . m)
            ("CONFIG_CHARGER_MACSMC" . #t)
            ("CONFIG_COMMON_CLK_APPLE_NCO" . #t)
            ("CONFIG_DRM_ADP" . m)
            ("CONFIG_DRM_APPLE" . m)
            ("CONFIG_DRM_APPLE_AUDIO" . #t)
            ("CONFIG_GPIO_MACSMC" . #t)
            ("CONFIG_HID_DOCKCHANNEL" . m)
            ("CONFIG_I2C_APPLE" . m)
            ("CONFIG_INPUT_MACSMC_HID" . #t)
            ("CONFIG_MFD_APPLE_SPMI_PMU" . m)
            ("CONFIG_MUX_APPLE_DPXBAR" . m)
            ("CONFIG_NVMEM_APPLE_EFUSES" . m)
            ("CONFIG_NVMEM_SPMI_MFD" . m)
            ("CONFIG_NVME_APPLE" . m)
            ("CONFIG_PCIE_APPLE" . m)
            ("CONFIG_PHY_APPLE_ATC" . m)
            ("CONFIG_PHY_APPLE_DPTX" . m)
            ("CONFIG_PINCTRL_APPLE_GPIO" . m)
            ("CONFIG_POWER_RESET_MACSMC" . #t)
            ("CONFIG_PWM_APPLE" . #t)
            ("CONFIG_RTC_DRV_MACSMC" . m)
            ("CONFIG_SENSORS_MACSMC" . m)
            ("CONFIG_SND_SOC_APPLE_MACAUDIO" . m)
            ("CONFIG_SND_SOC_APPLE_MCA" . m)
            ("CONFIG_SND_SOC_CS42L84" . m)
            ("CONFIG_SPI_APPLE" . m)
            ("CONFIG_SPI_HID_APPLE_OF" . m)
            ("CONFIG_SPMI_APPLE" . m)
            ("CONFIG_TOUCHSCREEN_APPLE_Z2" . m)
            ("CONFIG_VIDEO_APPLE_ISP" . m))
          %config))

(define %edge-config
  (append '(("CONFIG_DRM" . #t)
            ("CONFIG_DRM_ACCEL" . #t)
            ("CONFIG_DRM_ASAHI" . m)
            ("CONFIG_RUST" . #t)
            ("CONFIG_RUST_BUILD_ASSERT_ALLOW" . #f)
            ("CONFIG_RUST_DEBUG_ASSERTIONS" . #f)
            ("CONFIG_RUST_OVERFLOW_CHECKS" . #t))
          %base-config))

(define linux-srcarch
  (@@ (gnu packages linux) linux-srcarch))

(define (make-asahi-linux-source commit hash patches)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/AsahiLinux/linux.git")
          (commit commit)))
    (file-name (git-file-name "linux-source" commit))
    (patches patches)
    (sha256 (base32 hash))))

(define asahi-linux-source
  (make-asahi-linux-source
   "asahi-6.11.6-2" "0a06jjp2a75rn7hpy4lmpngjc0yddscbi611z8lisbwl5p1y8rf0"
   (list)))

(define* (make-asahi-linux name
                           #:key
                           defconfig
                           (configs (map config-format-line %base-config))
                           (extra-version #f)
                           (linux linux-libre-arm64-generic)
                           (source asahi-linux-source)
                           (version "6.11.6-2-asahi"))
  (package
    (inherit linux)
    (name (or name (package-name linux)))
    (source (or source (package-source linux)))
    (arguments
     (substitute-keyword-arguments (package-arguments linux)
       ((#:imported-modules imported-modules %default-gnu-imported-modules)
        `((guix build kconfig) ,@imported-modules))
       ((#:modules modules)
        `((guix build kconfig) ,@modules))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'configure 'configure-bindgen
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((bindgen (false-if-exception
                                (search-input-file inputs "bin/bindgen"))))
                  (when bindgen (setenv "BINDGEN" bindgen)))))
            (add-before 'configure 'configure-libclang
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((clang (assoc-ref inputs "clang")))
                  (when clang
                    (setenv "CC" "clang")
                    (setenv "LIBCLANG_PATH" (string-append clang "/lib"))))))
            (add-before 'configure 'configure-rustc
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((rustc (false-if-exception (search-input-file inputs "bin/rustc"))))
                  (when rustc (setenv "RUSTC" rustc)))))
            (add-before 'configure 'configure-rust-src
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((source (false-if-exception
                               (search-input-directory inputs "lib/rustlib/src/rust/library"))))
                  (when source (setenv "RUST_LIB_SRC" source)))))
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys #:rest arguments)
                (setenv "EXTRAVERSION"
                        #$(and extra-version
                               (not (string-null? extra-version))
                               (string-append "-" extra-version)))
                (let* ((configs (string-append "arch/" #$(linux-srcarch)
                                               "/configs/"))
                       (guix_defconfig (string-append configs
                                                      "guix_defconfig")))
                  #$(cond
                     ((not defconfig)
                      #~(begin
                          ;; Call the original 'configure phase.
                          (apply (assoc-ref #$phases 'configure) arguments)
                          ;; Save a defconfig file.
                          (invoke "make" "savedefconfig")
                          ;; Move the saved defconfig to the proper location.
                          (rename-file "defconfig"
                                       guix_defconfig)))
                     ((string? defconfig)
                      ;; Use another existing defconfig from the Linux sources.
                      #~(rename-file (string-append configs #$defconfig)
                                     guix_defconfig))
                     (else
                      ;; Copy the defconfig input to the proper location.
                      #~(copy-file #$defconfig guix_defconfig)))
                  (chmod guix_defconfig #o644)
                  (modify-defconfig guix_defconfig '#$configs)
                  (invoke "make" "guix_defconfig"))))))))))

(define-public asahi-linux
  (make-asahi-linux "asahi-linux"))

(define-public asahi-linux-edge
  (let* ((configs (map config-format-line %edge-config))
         (base (make-asahi-linux "asahi-linux-edge" #:configs configs)))
    (package
      (inherit base)
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend python
                  rust
                  rust-src
                  rust-bindgen-cli
                  zstd))))))

(define-public asahi-alsa-ucm-conf
  (package
    (name "asahi-alsa-ucm-conf")
    (version "5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AsahiLinux/alsa-ucm-conf-asahi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "072gw5mbi8wgjh8f8gddqcf8pn3fsspsl4zd639ggb0lkb7hv9bm"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("ucm2" "share/alsa/ucm2"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'add-alsa-ucm-conf
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (alsa-ucm-conf (assoc-ref inputs "alsa-ucm-conf")))
                (for-each (lambda (dir)
                            (let ((path (format #f "/share/alsa/~a" dir)))
                              (copy-recursively
                               (string-append alsa-ucm-conf path)
                               (string-append out path)
                               #:follow-symlinks? #t)))
                          '("ucm" "ucm2"))))))))
    (native-inputs
     `(("alsa-ucm-conf" ,linux:alsa-ucm-conf)))
    (home-page "https://github.com/AsahiLinux/alsa-ucm-conf-asahi")
    (synopsis "The Advanced Linux Sound Architecture Use Case Manager")
    (description
     "This package contains Advanced Linux Sound Architecture Use Case Manager
configuration of audio input/output names and routing for specific audio
hardware.")
    (license license:bsd-3)))

(define replace-alsa-ucm-conf
  (package-input-rewriting/spec
   `(("alsa-ucm-conf" . ,(const asahi-alsa-ucm-conf)))))

(define-public asahi-alsa-lib
  (package
    (inherit (replace-alsa-ucm-conf linux:alsa-lib))
    (name "asahi-alsa-lib")))

(define-public replace-alsa-lib
  (package-input-rewriting/spec
   `(("alsa-lib" . ,(const asahi-alsa-lib)))))

(define-public asahi-alsa-utils
  (package
    (inherit (replace-alsa-ucm-conf linux:alsa-utils))
    (name "asahi-alsa-utils")))

(define-public asahi-alsa-plugins
  (package
    (inherit (replace-alsa-ucm-conf linux:alsa-plugins))
    (name "asahi-alsa-plugins")))

(define-public asahi-pipewire
  (let ((base (replace-alsa-ucm-conf linux:pipewire)))
    (package
      (inherit base)
      (name "asahi-pipewire")
      (inputs (modify-inputs (package-inputs base)
                (prepend lilv))))))

(define-public asahi-wireplumber
  (package
    (inherit (replace-alsa-ucm-conf linux:wireplumber))
    (name "asahi-wireplumber")))

(define-module (asahi guix packages linux)
  #:use-module ((gnu packages linux) #:prefix linux:)
  #:use-module ((guix licenses) #:prefix license:)
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


(define %configs
  '("CONFIG_APPLE_MFI_FASTCHARGE=m"
    "CONFIG_AQTION=m"
    "CONFIG_ARM64_16K_PAGES=y"
    "CONFIG_BCACHE=m"
    "CONFIG_BLK_DEV_DM=y"
    "CONFIG_BLK_DEV_MD=y"
    "CONFIG_BRCMFMAC=m"
    "CONFIG_BRCMFMAC_PCIE=y"
    "CONFIG_BRCMFMAC_PROTO_BCDC=y"
    "CONFIG_BRCMFMAC_PROTO_MSGBUF=y"
    "CONFIG_BRCMFMAC_USB=y"
    "CONFIG_BT=m"
    "CONFIG_BT_BNEP=m"
    "CONFIG_BT_BREDR=y"
    "CONFIG_BT_HCIUART_BCM=y"
    "CONFIG_BT_HIDP=m"
    "CONFIG_BT_LE=y"
    "CONFIG_BT_RFCOMM=m"
    "CONFIG_CFG80211=m"
    "CONFIG_CPU_FREQ_DEFAULT_GOV_SCHEDUTIL=y"
    "CONFIG_CRYPTO_SERPENT=m"
    "CONFIG_CRYPTO_USER_API_AEAD=y"
    "CONFIG_CRYPTO_USER_API_HASH=y"
    "CONFIG_CRYPTO_USER_API_RNG=y"
    "CONFIG_CRYPTO_USER_API_SKCIPHER=y"
    "CONFIG_CRYPTO_WP512=m"
    "CONFIG_DM_CACHE=m"
    "CONFIG_DM_CRYPT=m"
    "CONFIG_DM_CRYPT=m"
    "CONFIG_DM_DEBUG=y"
    "CONFIG_DM_DELAY=m"
    "CONFIG_DM_DUST=m"
    "CONFIG_DM_FLAKEY=m"
    "CONFIG_DM_INIT=y"
    "CONFIG_DM_INTEGRITY=m"
    "CONFIG_DM_LOG_USERSPACE=m"
    "CONFIG_DM_LOG_WRITES=m"
    "CONFIG_DM_MIRROR=y"
    "CONFIG_DM_MULTIPATH=m"
    "CONFIG_DM_MULTIPATH_QL=m"
    "CONFIG_DM_MULTIPATH_ST=m"
    "CONFIG_DM_RAID=m"
    "CONFIG_DM_SNAPSHOT=y"
    "CONFIG_DM_SWITCH=m"
    "CONFIG_DM_THIN_PROVISIONING=m"
    "CONFIG_DM_UEVENT=y"
    "CONFIG_DM_VERITY=m"
    "CONFIG_DM_VERITY_FEC=y"
    "CONFIG_DM_WRITECACHE=m"
    "CONFIG_DM_ZERO=y"
    "CONFIG_DM_ZONED=m"
    "CONFIG_DRM_ASAHI_DEBUG_ALLOCATOR"
    "CONFIG_DRM_CIRRUS_QEMU=m"
    "CONFIG_DRM_FBDEV_EMULATION=y"
    "CONFIG_DRM_GEM_SHMEM_HELPER=y"
    "CONFIG_DRM_SCHED=y"
    "CONFIG_DRM_VGEM"
    "CONFIG_ENERGY_MODEL=y"
    "CONFIG_GCC_PLUGINS"
    "CONFIG_HIBERNATION"
    "CONFIG_HID_APPLE=m"
    "CONFIG_HID_BATTERY_STRENGTH=y"
    "CONFIG_HID_MAGICMOUSE=m"
    "CONFIG_INPUT_LEDS=y"
    "CONFIG_LEDS_PWM=y"
    "CONFIG_MD=y"
    "CONFIG_MD_CLUSTER=m"
    "CONFIG_MMC_CQHCI=m"
    "CONFIG_MMC_SDHCI=m"
    "CONFIG_MMC_SDHCI_PCI=m"
    "CONFIG_MOUSE_APPLETOUCH=m"
    "CONFIG_NET_VENDOR_AQUANTIA=y"
    "CONFIG_NET_VENDOR_BROADCOM=y"
    "CONFIG_NLS_ISO8859_1=m"
    "CONFIG_POWER_RESET_GPIO=y"
    "CONFIG_REGULATOR_FIXED_VOLTAGE=y"
    "CONFIG_SERIAL_SAMSUNG=y"
    "CONFIG_SERIAL_SAMSUNG_CONSOLE=y"
    "CONFIG_SND_SOC_CS42L83=m"
    "CONFIG_SND_SOC_TAS2764=m"
    "CONFIG_SND_SOC_TAS2770=m"
    "CONFIG_SUSPEND=y"
    "CONFIG_TIGON3=m"
    "CONFIG_TYPEC_TPS6598X=m"
    "CONFIG_UCLAMP_TASK=y"
    "CONFIG_UCLAMP_TASK_GROUP=y"
    "CONFIG_USB_DWC3=m"
    "CONFIG_USB_DWC3_DUAL_ROLE=y"
    "CONFIG_USB_HID=m"
    "CONFIG_USB_STORAGE=m"
    "CONFIG_USB_UAS=m"
    "CONFIG_USB_XHCI_HCD=m"
    "CONFIG_USB_XHCI_PCI_ASMEDIA=y"
    "CONFIG_USB_XHCI_PCI_RENESAS=y"
    "CONFIG_USB_XHCI_PLATFORM=m"
    "CONFIG_WATCHDOG_HANDLE_BOOT_ENABLED=y"
    "CONFIG_WLAN_VENDOR_BROADCOM=y"))

(define %base-configs
  (cons* "CONFIG_APPLE_ADMAC=y"
         "CONFIG_APPLE_AIC=y"
         "CONFIG_APPLE_DART=m"
         "CONFIG_APPLE_DOCKCHANNEL=m"
         "CONFIG_APPLE_M1_CPU_PMU=y"
         "CONFIG_APPLE_MAILBOX=y"
         "CONFIG_APPLE_PLATFORMS=y"
         "CONFIG_APPLE_PMGR_MISC=y"
         "CONFIG_APPLE_PMGR_PWRSTATE=y"
         "CONFIG_APPLE_RTKIT=m"
         "CONFIG_APPLE_RTKIT_HELPER=m"
         "CONFIG_APPLE_SART=y"
         "CONFIG_APPLE_SIO=m"
         "CONFIG_APPLE_SMC=m"
         "CONFIG_APPLE_SMC_RTKIT=m"
         "CONFIG_APPLE_WATCHDOG=y"
         "CONFIG_ARCH_APPLE=y"
         "CONFIG_ARM64_MEMORY_MODEL_CONTROL=y"
         "CONFIG_ARM_APPLE_CPUIDLE=y"
         "CONFIG_ARM_APPLE_SOC_CPUFREQ=y"
         "CONFIG_BT_HCIBCM4377=m"
         "CONFIG_CHARGER_MACSMC=y"
         "CONFIG_COMMON_CLK_APPLE_NCO=y"
         "CONFIG_DRM_ADP=m"
         "CONFIG_DRM_APPLE=m"
         "CONFIG_DRM_APPLE_AUDIO=y"
         "CONFIG_GPIO_MACSMC=y"
         "CONFIG_HID_DOCKCHANNEL=m"
         "CONFIG_I2C_APPLE=m"
         "CONFIG_INPUT_MACSMC_HID=y"
         "CONFIG_MFD_APPLE_SPMI_PMU=m"
         "CONFIG_MUX_APPLE_DPXBAR=m"
         "CONFIG_NVMEM_APPLE_EFUSES=m"
         "CONFIG_NVMEM_SPMI_MFD=m"
         "CONFIG_NVME_APPLE=m"
         "CONFIG_PCIE_APPLE=m"
         "CONFIG_PHY_APPLE_ATC=m"
         "CONFIG_PHY_APPLE_DPTX=m"
         "CONFIG_PINCTRL_APPLE_GPIO=m"
         "CONFIG_POWER_RESET_MACSMC=y"
         "CONFIG_PWM_APPLE=y"
         "CONFIG_RTC_DRV_MACSMC=m"
         "CONFIG_SENSORS_MACSMC=m"
         "CONFIG_SND_SOC_APPLE_MACAUDIO=m"
         "CONFIG_SND_SOC_APPLE_MCA=m"
         "CONFIG_SND_SOC_CS42L84=m"
         "CONFIG_SPI_APPLE=m"
         "CONFIG_SPI_HID_APPLE_OF=m"
         "CONFIG_SPMI_APPLE=m"
         "CONFIG_TOUCHSCREEN_APPLE_Z2=m"
         "CONFIG_VIDEO_APPLE_ISP=m"
         %configs))

(define %edge-configs
  (cons* "CONFIG_DRM=y"
         "CONFIG_DRM_ACCEL=y"
         "CONFIG_DRM_ASAHI=m"
         "CONFIG_RUST=y"
         "CONFIG_RUST_BUILD_ASSERT_ALLOW"
         "CONFIG_RUST_DEBUG_ASSERTIONS"
         "CONFIG_RUST_OVERFLOW_CHECKS=y"
         %base-configs))

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
                           (configs %base-configs)
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
  (let ((base (make-asahi-linux "asahi-linux-edge" #:configs %edge-configs)))
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

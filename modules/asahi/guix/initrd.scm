(define-module (asahi guix initrd)
  #:use-module (asahi guix build firmware)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages linux)
  #:use-module (gnu system linux-initrd)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:export (asahi-initrd-modules))

(define initrd-modules
  (list "cirrus" ;; for guix system vm
        "dm-crypt"
        "hid-apple"
        "nls_iso8859-1"
        "serpent_generic"
        "uas"
        "usb-storage"
        "usbhid"
        "wp512"
        "xhci-plat-hcd"))

(define asahi-initrd-modules
  (cons*
   ;; For NVMe & SMC
   ;; "apple-mailbox"
   ;; For NVMe
   "nvme-apple"
   ;; For USB and HID
   "pinctrl-apple-gpio"
   ;; SMC core
   "macsmc"
   "macsmc-rtkit"
   ;; For USB
   "apple-dart"
   "dwc3"
   "dwc3-of-simple"
   "gpio_macsmc"
   "i2c-pasemi-platform"
   "nvmem-apple-efuses"
   "pcie-apple"
   "phy-apple-atc"
   "tps6598x"
   "xhci-pci"
   ;; For HID
   "spi-apple"
   "spi-hid-apple"
   "spi-hid-apple-of"
   ;; For RTC
   "nvmem_spmi_mfd"
   "rtc-macsmc"
   "simple-mfd-spmi"
   "spmi-apple-controller"
   ;; For MTP HID
   "apple-dockchannel"
   "apple-rtkit-helper"
   "dockchannel-hid"
   initrd-modules))

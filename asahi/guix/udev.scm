(define-module (asahi guix udev)
  #:use-module (gnu services base)
  #:export (%asahi-udev-rules
            %udev-backlight-brightness-rule
            %udev-kbd-backlight-brightness-rule))

(define %udev-backlight-brightness-rule
  (udev-rule
   "90-backlight-brightness.rules"
   (string-append
    "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
    "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
    "\n"
    "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
    "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %udev-kbd-backlight-brightness-rule
  (udev-rule
   "90-kbd-backlight-brightness.rules"
   (string-append
    "ACTION==\"add\", SUBSYSTEM==\"leds\", "
    "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/leds/kbd_backlight/brightness\""
    "\n"
    "ACTION==\"add\", SUBSYSTEM==\"leds\", "
    "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/leds/kbd_backlight/brightness\"")))

(define %asahi-udev-rules
  (list %udev-backlight-brightness-rule
        %udev-kbd-backlight-brightness-rule))

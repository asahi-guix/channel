(define-module (tests asahi guix installer script)
  #:use-module (asahi guix installer script)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-installer-script")

(define source-script
  "tests/asahi/guix/installer/alx.sh")

(define target-script
  "/tmp/asahi-guix-installer.sh")

(test-begin suite)

(test-equal "read installer script"
  (installer-script
   (content (call-with-input-file source-script get-string-all))
   (installer-base "https://cdn.asahilinux.org/installer")
   (installer-data "https://github.com/AsahiLinux/asahi-installer/raw/prod/data/installer_data.json")
   (installer-data-alt "https://alx.sh/installer_data.json")
   (repo-base "https://cdn.asahilinux.org")
   (report "https://stats.asahilinux.org/report")
   (report-tag "alx-prod")
   (version-flag "https://cdn.asahilinux.org/installer/latest"))
  (read-installer-script source-script))

(test-equal "write installer script"
  (read-installer-script source-script)
  (begin
    (write-installer-script (read-installer-script source-script) target-script)
    (read-installer-script target-script)))

(test-equal "read, modify and write installer script"
  (begin
    (write-installer-script
     (installer-script
      (inherit (read-installer-script source-script))
      (installer-base "new-installer-base")
      (installer-data "new-installer-data")
      (installer-data-alt "new-installer-data-alt")
      (repo-base "new-repo-base")
      (report "new-report")
      (report-tag "new-report-tag")
      (version-flag "new-version-flag"))
     target-script)
    (read-installer-script target-script))
  (installer-script
   (content (call-with-input-file target-script get-string-all))
   (installer-base "new-installer-base")
   (installer-data "new-installer-data")
   (installer-data-alt "new-installer-data-alt")
   (repo-base "new-repo-base")
   (report "new-report")
   (report-tag "new-report-tag")
   (version-flag "new-version-flag")))

(test-end suite)

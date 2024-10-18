(define-module (asahi guix installer script)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:export (installer-script
            installer-script-content
            installer-script-installer-base
            installer-script-installer-data
            installer-script-installer-data-alt
            installer-script-repo-base
            installer-script-report
            installer-script-report-tag
            installer-script-version-flag
            installer-script?
            make-installer-script
            read-installer-script
            write-installer-script))

(define-record-type* <installer-script>
  installer-script
  make-installer-script
  installer-script?
  (content installer-script-content)
  (installer-base installer-script-installer-base)
  (installer-data installer-script-installer-data)
  (installer-data-alt installer-script-installer-data-alt)
  (repo-base installer-script-repo-base)
  (report installer-script-report)
  (report-tag installer-script-report-tag)
  (version-flag installer-script-version-flag))

(define (extract-field content field)
  (let ((matches (string-match (format #f "export ~a=\"?([^\n\"]+)\"?" field) content)))
    (when (regexp-match? matches)
      (match:substring matches 1))))

(define (read-installer-script filename)
  (installer-script
   (content (call-with-input-file filename get-string-all))
   (installer-base (extract-field content "INSTALLER_BASE"))
   (installer-data (extract-field content "INSTALLER_DATA"))
   (installer-data-alt (extract-field content "INSTALLER_DATA_ALT"))
   (repo-base (extract-field content "REPO_BASE"))
   (report (extract-field content "REPORT"))
   (report-tag (extract-field content "REPORT_TAG"))
   (version-flag (extract-field content "VERSION_FLAG"))))

(define (write-installer-script script filename)
  (mkdir-p (dirname filename))
  (call-with-output-file filename
    (lambda (port)
      (set-port-encoding! port "UTF-8")
      (format port "~a" (installer-script-content script))))
  (substitute* filename
    (("export INSTALLER_BASE=.*")
     (format #f "export INSTALLER_BASE=~a\n" (installer-script-installer-base script)))
    (("export INSTALLER_DATA=.*")
     (format #f "export INSTALLER_DATA=~a\n" (installer-script-installer-data script)))
    (("export INSTALLER_DATA_ALT=.*")
     (format #f "export INSTALLER_DATA_ALT=~a\n" (installer-script-installer-data-alt script)))
    (("export REPO_BASE=.*")
     (format #f "export REPO_BASE=~a\n" (installer-script-repo-base script)))
    (("export REPORT=.*")
     (format #f "export REPORT=~a\n" (installer-script-report script)))
    (("export REPORT_TAG=.*")
     (format #f "export REPORT_TAG=~a\n" (installer-script-report-tag script)))
    (("export VERSION_FLAG=.*")
     (format #f "export VERSION_FLAG=~a\n" (installer-script-version-flag script))))
  (chmod filename #o755)
  filename)

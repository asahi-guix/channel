(define-module (asahi guix packages)
  #:use-module (gnu packages)
  #:use-module (guix diagnostics)
  #:use-module (guix packages)
  #:export (asahi-package?
            asahi-packages))

(define (asahi-package? package)
  (let ((filename (location-file (package-location package))))
    (string-prefix? "asahi/guix/packages" filename)))

(define (asahi-packages)
  (fold-packages (lambda (package result)
                   (if (asahi-package? package)
                       (cons package result)
                       result))
                 '()
                 #:select? (const #t)))

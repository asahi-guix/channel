(define-module (asahi guix build installer metadata)
  #:use-module (asahi guix build installer os)
  #:use-module (asahi guix build utils)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (json)
  #:export (installer-metadata
            installer-metadata->json-alist
            installer-metadata-os-list
            installer-metadata?
            json-alist->installer-metadata
            make-installer-metadata
            merge-installer-metadata
            read-installer-metadata
            write-installer-metadata))

(define-record-type* <installer-metadata>
  installer-metadata
  make-installer-metadata
  installer-metadata?
  (os-list installer-metadata-os-list))

(define (installer-metadata->json-alist data)
  (define os-list
    (map (lambda (os)
           (installer-os->json-alist os))
         (installer-metadata-os-list data)))
  `(("os_list" . ,(apply vector os-list))))

(define (json-alist->installer-metadata alist)
  (installer-metadata
   (os-list
    (map json-alist->installer-os
         (vector->list (assoc-ref alist "os_list"))))))

(define (merge-installer-metadata data-1 data-2)
  (installer-metadata
   (os-list (append (installer-metadata-os-list data-1)
                    (installer-metadata-os-list data-2)))))

(define (read-installer-metadata filename)
  (call-with-input-file filename
    (lambda (port)
      (json-alist->installer-metadata (json->scm port)))))

(define (write-installer-metadata data filename)
  (let ((content (scm->json-string (installer-metadata->json-alist data) #:pretty #t)))
    (mkdir-p (dirname filename))
    (call-with-output-file filename
      (lambda (port)
        (set-port-encoding! port "UTF-8")
        (format port "~a\n" content)))
    data))

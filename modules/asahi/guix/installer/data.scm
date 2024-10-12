(define-module (asahi guix installer data)
  #:use-module (asahi guix installer os)
  #:use-module (asahi guix build utils)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (json)
  #:export (installer-data
            installer-data->json-alist
            installer-data-os-list
            installer-data?
            json-alist->installer-data
            make-installer-data
            merge-installer-data
            read-installer-data
            write-installer-data))

(define-record-type* <installer-data>
  installer-data
  make-installer-data
  installer-data?
  (os-list installer-data-os-list))

(define (installer-data->json-alist data)
  (define os-list
    (map (lambda (os)
           (installer-os->json-alist os))
         (installer-data-os-list data)))
  `(("os_list" . ,(apply vector os-list))))

(define (json-alist->installer-data alist)
  (installer-data
   (os-list
    (map json-alist->installer-os
         (vector->list (assoc-ref alist "os_list"))))))

(define (merge-installer-data data-1 data-2)
  (installer-data
   (os-list (append (installer-data-os-list data-1)
                    (installer-data-os-list data-2)))))

(define (read-installer-data filename)
  (call-with-input-file filename
    (lambda (port)
      (json-alist->installer-data (json->scm port)))))

(define (write-installer-data data filename)
  (let ((content (scm->json-string (installer-data->json-alist data) #:pretty #t)))
    (mkdir-p (dirname filename))
    (call-with-output-file filename
      (lambda (port)
        (set-port-encoding! port "UTF-8")
        (format port "~a\n" content)))
    data))

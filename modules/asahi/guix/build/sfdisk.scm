(define-module (asahi guix build sfdisk)
  #:use-module (asahi guix build utils)
  #:use-module (guix records)
  #:use-module (json)
  #:export (make-sfdisk-partition
            make-sfdisk-table
            sfdisk-efi-partition?
            sfdisk-linux-partition?
            sfdisk-list
            sfdisk-parse
            sfdisk-partition
            sfdisk-partition-attrs
            sfdisk-partition-boot?
            sfdisk-partition-device
            sfdisk-partition-end
            sfdisk-partition-id
            sfdisk-partition-name
            sfdisk-partition-sectors
            sfdisk-partition-size
            sfdisk-partition-start
            sfdisk-partition-type
            sfdisk-partition?
            sfdisk-table
            sfdisk-table-device
            sfdisk-table-first-lba
            sfdisk-table-id
            sfdisk-table-label
            sfdisk-table-last-lba
            sfdisk-table-partitions
            sfdisk-table-sector-size
            sfdisk-table-unit
            sfdisk-table?))

(define-record-type* <sfdisk-table>
  sfdisk-table
  make-sfdisk-table
  sfdisk-table?
  (device sfdisk-table-device)
  (first-lba sfdisk-table-first-lba)
  (id sfdisk-table-id)
  (label sfdisk-table-label)
  (last-lba sfdisk-table-last-lba)
  (partitions sfdisk-table-partitions)
  (sector-size sfdisk-table-sector-size)
  (unit sfdisk-table-unit))

(define-record-type* <sfdisk-partition>
  sfdisk-partition
  make-sfdisk-partition
  sfdisk-partition?
  (attrs sfdisk-partition-attrs (default #f))
  (name sfdisk-partition-name)
  (node sfdisk-partition-node)
  (size sfdisk-partition-size)
  (start sfdisk-partition-start)
  (type sfdisk-partition-type)
  (uuid sfdisk-partition-uuid))

(define (sfdisk-efi-partition? partition)
  (equal? "C12A7328-F81F-11D2-BA4B-00A0C93EC93B"
          (sfdisk-partition-type partition)))

(define (sfdisk-linux-partition? partition)
  (equal? "0FC63DAF-8483-4772-8E79-3D69D8477DE4"
          (sfdisk-partition-type partition)))

(define (sfdisk-json->partition partition)
  (sfdisk-partition
   (attrs (assoc-ref partition "attrs"))
   (name (assoc-ref partition "name"))
   (node (assoc-ref partition "node"))
   (size (assoc-ref partition "size"))
   (start (assoc-ref partition "start"))
   (type (assoc-ref partition "type"))
   (uuid (assoc-ref partition "uuid"))))

(define (sfdisk-json->table data)
  (let ((table (assoc-ref data "partitiontable")))
    (sfdisk-table
     (device (assoc-ref table "device"))
     (first-lba (assoc-ref table "firstlba"))
     (id (assoc-ref table "id"))
     (label (assoc-ref table "label"))
     (last-lba (assoc-ref table "lastlba"))
     (sector-size (assoc-ref table "sectorsize"))
     (unit (assoc-ref table "unit"))
     (partitions (map sfdisk-json->partition
                      (vector->list (assoc-ref table "partitions")))))))

(define (sfdisk-parse output)
  (sfdisk-json->table (json-string->scm output)))

(define (sfdisk-command filename)
  (list "sfdisk" "--color=never" "--json" "--dump" filename))

(define (sfdisk-list filename)
  (sfdisk-parse (apply command-output (sfdisk-command filename))))

;; (sfdisk-list "/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image")

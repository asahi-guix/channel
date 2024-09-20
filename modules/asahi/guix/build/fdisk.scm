(define-module (asahi guix build fdisk)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (fdisk-list
            fdisk-parse
            fdisk-partition
            fdisk-partition-boot?
            fdisk-partition-device
            fdisk-partition-end
            fdisk-partition-id
            fdisk-partition-sectors
            fdisk-partition-size
            fdisk-partition-start
            fdisk-partition-type
            fdisk-partition?
            fdisk-disk
            fdisk-disk-partitions
            fdisk-disk?
            make-fdisk-partition
            make-fdisk-disk))

(define fdisk-partition-regex
  "^([^ ]+)\\s(\\*| )\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([^ ]+)\\s+([^ ]+)\\s+(.*)$")

(define-record-type* <fdisk-partition>
  fdisk-partition
  make-fdisk-partition
  fdisk-partition?
  (boot? fdisk-partition-boot?)
  (device fdisk-partition-device)
  (end fdisk-partition-end)
  (id fdisk-partition-id)
  (sectors fdisk-partition-sectors)
  (size fdisk-partition-size)
  (start fdisk-partition-start)
  (type fdisk-partition-type))

(define-record-type* <fdisk-disk>
  fdisk-disk
  make-fdisk-disk
  fdisk-disk?
  (partitions fdisk-disk-partitions))

(define (command-output cmd . args)
  "Execute CMD with ARGS and return its output without trailing newspace."
  (let* ((port (apply open-pipe* OPEN_READ cmd args))
         (output (read-string port)))
    (close-port port)
    (string-trim-right output #\newline)))

(define (fdisk-parse-partition-boot? fields)
  (equal? "*" (match:substring fields 2)))

(define (fdisk-parse-partition-device fields)
  (match:substring fields 1))

(define (fdisk-parse-partition-end fields)
  (string->number (match:substring fields 4)))

(define (fdisk-parse-partition-id fields)
  (match:substring fields 7))

(define (fdisk-parse-partition-sectors fields)
  (string->number (match:substring fields 5)))

(define (fdisk-parse-partition-size fields)
  (match:substring fields 6))

(define (fdisk-parse-partition-start fields)
  (string->number (match:substring fields 3)))

(define (fdisk-parse-partition-type fields)
  (match:substring fields 8))

(define (fdisk-parse-partition-line line)
  (let ((fields (string-match fdisk-partition-regex line)))
    (when (regexp-match? fields)
      (fdisk-partition
       (device (fdisk-parse-partition-device fields))
       (boot? (fdisk-parse-partition-boot? fields))
       (start (fdisk-parse-partition-start fields))
       (end (fdisk-parse-partition-end fields))
       (sectors (fdisk-parse-partition-sectors fields))
       (size (fdisk-parse-partition-size fields))
       (id (fdisk-parse-partition-id fields))
       (type (fdisk-parse-partition-type fields))))))

(define (fdisk-parse-partitions output)
  (let ((lines (string-split output #\newline)))
    (filter fdisk-partition? (map fdisk-parse-partition-line lines))))

(define (fdisk-parse output)
  (fdisk-disk (partitions (fdisk-parse-partitions output))))

(define (fdisk-command filename)
  (list "fdisk" "-Lnever" "-lu" "-b" "4096" filename))

(define (fdisk-list filename)
  (fdisk-parse (apply command-output (fdisk-command filename))))

;; (fdisk-list "/gnu/store/06564cby4w29g1m5d273fa7zy6i62g9v-disk-image")

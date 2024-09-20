(define-module (tests asahi guix build fdisk)
  #:use-module (asahi guix build fdisk)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-fdisk")

(define fdisk-output
  "Disk /gnu/store/06564cby4w29g1m5d273fa7zy6i62g9v-disk-image: 2.03 GiB, 2182320128 bytes, 532793 sectors
Units: sectors of 1 * 4096 = 4096 bytes
Sector size (logical/physical): 4096 bytes / 4096 bytes
I/O size (minimum/optimal): 4096 bytes / 4096 bytes
Disklabel type: dos
Disk identifier: 0x00000000

Device                                                  Boot Start     End Sectors  Size Id Type
/gnu/store/06564cby4w29g1m5d273fa7zy6i62g9v-disk-image1       2048   83967   81920  320M ef EFI (FAT-12/16/32)
/gnu/store/06564cby4w29g1m5d273fa7zy6i62g9v-disk-image2 *    83968 4262343 4178376 15.9G 83 Linux
")

(test-begin suite)

(test-equal "list block devices"
  (fdisk-disk
   (partitions
    (list
     (fdisk-partition
      (boot? #f)
      (device "/gnu/store/06564cby4w29g1m5d273fa7zy6i62g9v-disk-image1")
      (end 83967)
      (id "ef")
      (sectors 81920)
      (size "320M")
      (start 2048)
      (type "EFI (FAT-12/16/32)"))
     (fdisk-partition
      (boot? #t)
      (device "/gnu/store/06564cby4w29g1m5d273fa7zy6i62g9v-disk-image2")
      (end 4262343)
      (id "83")
      (sectors 4178376)
      (size "15.9G")
      (start 83968)
      (type "Linux")))))
  (fdisk-parse fdisk-output))

(test-end suite)

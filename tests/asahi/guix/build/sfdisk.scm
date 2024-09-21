(define-module (tests asahi guix build sfdisk)
  #:use-module (asahi guix build sfdisk)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-sfdisk")

(define sfdisk-output
  "{\"partitiontable\": {
      \"device\": \"/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image\",
      \"firstlba\": 2048,
      \"id\": \"C46BD3E2-E70E-40BD-8912-0A1A42BAF5BF\",
      \"label\": \"gpt\",
      \"lastlba\": 4192814,
      \"sectorsize\": 512,
      \"unit\": \"sectors\",
      \"partitions\": [
         {
            \"node\": \"/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image1\",
            \"start\": 2048,
            \"size\": 81920,
            \"type\": \"C12A7328-F81F-11D2-BA4B-00A0C93EC93B\",
            \"uuid\": \"1718E4C8-6FEF-4641-83E5-5BAAB3EF75F0\",
            \"name\": \"GNU-ESP\"
         },{
            \"node\": \"/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image2\",
            \"start\": 83968,
            \"size\": 4108840,
            \"type\": \"0FC63DAF-8483-4772-8E79-3D69D8477DE4\",
            \"uuid\": \"BA6928AB-09B7-4AE6-9870-4FAFB5612F49\",
            \"name\": \"Guix_image\",
            \"attrs\": \"LegacyBIOSBootable\"}]}}")

(test-begin suite)

(test-equal "list block devices"
  (sfdisk-table
   (device "/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image")
   (first-lba 2048)
   (id "C46BD3E2-E70E-40BD-8912-0A1A42BAF5BF")
   (label "gpt")
   (last-lba 4192814)
   (sector-size 512)
   (unit "sectors")
   (partitions
    (list
     (sfdisk-partition
      (node "/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image1")
      (start 2048)
      (size 81920)
      (type "C12A7328-F81F-11D2-BA4B-00A0C93EC93B")
      (uuid "1718E4C8-6FEF-4641-83E5-5BAAB3EF75F0")
      (name "GNU-ESP"))
     (sfdisk-partition
      (node "/gnu/store/hfr97d38hpgq2skh10192f1ik1smvrx7-asahi-base-image2")
      (start 83968)
      (size 4108840)
      (type "0FC63DAF-8483-4772-8E79-3D69D8477DE4")
      (uuid "BA6928AB-09B7-4AE6-9870-4FAFB5612F49")
      (name "Guix_image")
      (attrs "LegacyBIOSBootable")))))
  (sfdisk-parse sfdisk-output))

(test-end suite)

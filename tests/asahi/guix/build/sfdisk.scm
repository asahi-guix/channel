(define-module (tests asahi guix build sfdisk)
  #:use-module (asahi guix build sfdisk)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-sfdisk")

(define sfdisk-output
  "{\"partitiontable\": {
      \"label\": \"gpt\",
      \"id\": \"32B576CC-45BD-4760-A057-46DDEED2845F\",
      \"device\": \"/dev/nvme0n1\",
      \"unit\": \"sectors\",
      \"firstlba\": 34,
      \"lastlba\": 1000215182,
      \"sectorsize\": 512,
      \"partitions\": [
         {
            \"node\": \"/dev/nvme0n1p1\",
            \"start\": 2048,
            \"size\": 2201600,
            \"type\": \"C12A7328-F81F-11D2-BA4B-00A0C93EC93B\",
            \"uuid\": \"72908D0A-9117-4348-89CF-17093307A778\"
         },{
            \"node\": \"/dev/nvme0n1p2\",
            \"start\": 2203648,
            \"size\": 4194304,
            \"type\": \"0FC63DAF-8483-4772-8E79-3D69D8477DE4\",
            \"uuid\": \"BC7DCEC7-D9D5-43B6-AD8F-374E4A99A5EC\"
         },{
            \"node\": \"/dev/nvme0n1p3\",
            \"start\": 6397952,
            \"size\": 993814528,
            \"type\": \"0FC63DAF-8483-4772-8E79-3D69D8477DE4\",
            \"uuid\": \"1376A27E-A080-4F78-A2F8-ED7F5D59EB7E\"}]}}")

(test-begin suite)

(test-equal "list block devices"
  (sfdisk-table
   (device "/dev/nvme0n1")
   (first-lba 34)
   (id "32B576CC-45BD-4760-A057-46DDEED2845F")
   (label "gpt")
   (last-lba 1000215182)
   (sector-size 512)
   (unit "sectors")
   (partitions
    (list
     (sfdisk-partition
      (node "/dev/nvme0n1p1")
      (start 2048)
      (size 2201600)
      (type "C12A7328-F81F-11D2-BA4B-00A0C93EC93B")
      (uuid "72908D0A-9117-4348-89CF-17093307A778"))
     (sfdisk-partition
      (node "/dev/nvme0n1p2")
      (start 2203648)
      (size 4194304)
      (type "0FC63DAF-8483-4772-8E79-3D69D8477DE4")
      (uuid "BC7DCEC7-D9D5-43B6-AD8F-374E4A99A5EC"))
     (sfdisk-partition
      (node "/dev/nvme0n1p3")
      (start 6397952)
      (size 993814528)
      (type "0FC63DAF-8483-4772-8E79-3D69D8477DE4")
      (uuid "1376A27E-A080-4F78-A2F8-ED7F5D59EB7E")))))
  (sfdisk-parse sfdisk-output))

(test-end suite)

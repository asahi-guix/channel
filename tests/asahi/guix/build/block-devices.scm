(define-module (tests asahi guix build block-devices)
  #:use-module (asahi guix build block-devices)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-block-devices")

(test-begin suite)

(test-assert "list block devices"
  (let ((devices (block-devices)))
    (or (null-list? devices)
        (every block-device? devices))))

(test-end suite)

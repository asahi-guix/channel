(define-module (tests asahi guix build utils)
  #:use-module (asahi guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-utils")

(test-begin suite)

(test-equal "command-output success"
  "Hello world"
  (command-output "echo" "Hello" "world"))

(test-error "command-output error"
            (command-output "x" "Hello" "world"))

(test-end suite)
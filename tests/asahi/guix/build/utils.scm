(define-module (tests asahi guix build utils)
  #:use-module (asahi guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-utils")

(test-begin suite)

(test-equal "capitalize with space"
  "Hello World" (capitalize "Hello world"))

(test-equal "capitalize with dash"
  "Hello World" (capitalize "hello-world" #\-))

(test-equal "command-output success"
  "Hello world"
  (command-output "echo" "Hello" "world"))

(test-error "command-output error"
  (command-output "x" "Hello" "world"))

(test-equal "escape whitespace in label"
  "EFI\\040-\\040ASAHI" (escape-label "EFI - ASAHI"))

(test-equal "config-parse-line-y"
  '("CONFIG_ARM64_16K_PAGES" . #t)
  (config-parse-line "CONFIG_ARM64_16K_PAGES=y"))

(test-equal "config-parse-line-n"
  '("CONFIG_ARM64_16K_PAGES" . #f)
  (config-parse-line "CONFIG_ARM64_16K_PAGES=n"))

(test-equal "config-parse-line-m"
  '("CONFIG_APPLE_MFI_FASTCHARGE" . m)
  (config-parse-line "CONFIG_APPLE_MFI_FASTCHARGE=m"))

(test-end suite)

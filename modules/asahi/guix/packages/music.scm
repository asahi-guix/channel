(define-module (asahi guix packages music)
  #:use-module ((gnu packages music) #:prefix music:)
  #:use-module (guix packages))

(define-public asahi-lsp-plugins
  (package
    (inherit music:lsp-plugins)
    (name "asahi-lsp-plugins")
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))))

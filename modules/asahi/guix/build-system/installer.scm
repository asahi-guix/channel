(define-module (asahi guix build-system installer)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (srfi srfi-11)
  #:export (%installer-build-system-modules
            default-glibc
            lower
            installer-build
            installer-build-system))

(define %installer-build-system-modules
  ;; Build-side modules imported by default.
  `((asahi guix build installer-build-system)
    (asahi guix build sfdisk)
    (asahi guix build utils)
    (asahi guix installer data)
    (asahi guix installer os)
    (asahi guix installer package)
    (asahi guix installer partition)
    (asahi guix search-paths)
    (guix records)
    (guix search-paths)
    ,@%default-gnu-imported-modules))

(define (default-glibc)
  "Return the default glibc packagey."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages base))))
    (module-ref module 'glibc)))

(define (installer-icon)
  "Return the default installer icon."
  (let ((module (resolve-interface '(asahi guix packages installer))))
    (module-ref module 'asahi-installer-icon)))

(define (installer-script)
  "Return the default installer script."
  (let ((module (resolve-interface '(asahi guix packages installer))))
    (module-ref module 'asahi-installer-script)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (glibc (default-glibc))
                (icon (installer-icon))
                (script (installer-script))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME from the given arguments."
  (define private-keywords
    '(#:target #:inputs #:native-inputs))
  (bag
    (name name)
    (system system)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs
                   ,@(standard-packages)
                   ,@`(("asahi-installer-icon" ,icon)
                       ("asahi-installer-script" ,script)
                       ("util-linux" ,util-linux)
                       ("zip" ,zip)
                       ("p7zip" ,p7zip))))
    (build-inputs native-inputs)
    (outputs outputs)
    (build installer-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (installer-build name inputs
                          #:key
                          guile source
                          (os-description "Asahi Guix")
                          (os-name "Asahi Guix")
                          (outputs '("out"))
                          (search-paths '())
                          (out-of-source? #t)
                          (tests? #t)
                          (validate-runpath? #t)
                          (patch-shebangs? #t)
                          (strip-binaries? #t)
                          (strip-flags %strip-flags)
                          (strip-directories %strip-directories)
                          (phases '(@ (asahi guix build installer-build-system)
                                      %standard-phases))
                          (system (%current-system))
                          (target #f)
                          (substitutable? #t)
                          (imported-modules %installer-build-system-modules)
                          (modules '((asahi guix build installer-build-system)
                                     (guix build utils))))
  (let-values (((name version) (package-name->name+version name #\-)))
    (define builder
      (with-extensions (list guile-json-4)
        (with-imported-modules imported-modules
          #~(begin
              (use-modules #$@modules)
              #$(with-build-variables inputs outputs
                  #~(installer-build
                     #:name #$name
                     #:version #$version
                     #:source #+source
                     #:system #$system
                     #:os-name #$os-name
                     #:os-description #$os-description
                     #:outputs %outputs
                     #:inputs %build-inputs
                     #:icon (string-append
                             (assoc-ref %build-inputs "asahi-installer-icon")
                             "/share/asahi-installer/asahi-guix.icns")
                     #:script (search-input-file %build-inputs "/bin/asahi-guix-installer.sh")
                     #:build-dir (string-append (getcwd) "/build")
                     #:search-paths '#$(sexp->gexp
                                        (map search-path-specification->sexp
                                             search-paths))
                     #:phases #$(if (pair? phases)
                                    (sexp->gexp phases)
                                    phases)
                     #:out-of-source? #$out-of-source?
                     #:tests? #$tests?
                     #:validate-runpath? #$validate-runpath?
                     #:patch-shebangs? #$patch-shebangs?
                     #:strip-binaries? #$strip-binaries?
                     #:strip-flags #$strip-flags
                     #:strip-directories #$strip-directories))))))

    (define guile-derivation
      (package->derivation (or guile (default-guile)) system #:graft? #f))

    (mlet %store-monad ((guile guile-derivation))
      (gexp->derivation name builder
                        #:system system
                        #:target #f
                        #:substitutable? substitutable?
                        #:graft? #f
                        #:guile-for-build guile))))

(define installer-build-system
  (build-system
    (name 'asahi-installer)
    (description "The Asahi Installer build system")
    (lower lower)))

(define-module (asahi guix services stats)
  #:use-module (asahi guix packages python-xyz)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python)
  #:use-module (gnu services base)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (asahi-stats-service-type))

(define-record-type* <asahi-stats-configuration>
  asahi-stats-configuration
  make-asahi-stats-configuration
  asahi-stats-configuration?
  (group asahi-stats-configuration-group
         (default "asahistats"))
  (log-file asahi-stats-configuration-log-file
            (default "/var/log/asahi-stats.log"))
  (package asahi-stats-configuration-package
           (default asahi-stats))
  (port asahi-stats-configuration-port
        (default 8000))
  (postgresql asahi-stats-configuration-postgresql
              (default postgresql))
  (user asahi-stats-configuration-user
        (default "asahistats")))

(define (asahi-stats-account config)
  "Return the user account and user group for CONFIG."
  (let ((user (asahi-stats-configuration-user config))
        (group (asahi-stats-configuration-group config)))
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "Asahi Installer Stats")
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (asahi-stats-create-schema config)
  "Return the command to create the Asahi Linux Stats schema for CONFIG."
  (let ((package (asahi-stats-configuration-package config))
        (postgresql (asahi-stats-configuration-postgresql config)))
    #~(list #$(file-append postgresql "/bin/psql")
            "-d" #$(asahi-stats-configuration-user config)
            "-f" #$(file-append package "/share/sql/schema.sql"))))

(define (asahi-stats-schema-service config)
  "Return the service that creates the Asahi Installer Stats schema for CONFIG."
  (let ((user (asahi-stats-configuration-user config))
        (group (asahi-stats-configuration-group config)))
    (shepherd-service
     (documentation "Asahi Installer Stats schema")
     (requirement '(postgres postgres-roles))
     (provision '(asahi-stats-schema))
     (one-shot? #t)
     (start #~(lambda args
                (let ((pid (fork+exec-command
                            #$(asahi-stats-create-schema config)
                            #:log-file #$(asahi-stats-configuration-log-file config)
                            #:group #$group
                            #:user #$user)))
                  (zero? (cdr (waitpid pid)))))))))

(define (asahi-stats-server-env config)
  "Return the environment variables for the Asahi Installer Stats server."
  (list (format #f "ASAHI_STATS_DB=~a" (asahi-stats-configuration-user config))
        (format #f "ASAHI_STATS_PORT=~a" (asahi-stats-configuration-port config))
        (format #f "ASAHI_STATS_USER=~a" (asahi-stats-configuration-user config))))

(define (asahi-stats-server-service config)
  "Return the service that runs the Asahi Installer Stats server for CONFIG."
  (let ((environment-variables (asahi-stats-server-env config))
        (package (asahi-stats-configuration-package config)))
    (shepherd-service
     (documentation "Asahi Installer Stats server")
     (provision '(asahi-stats))
     (requirement '(postgres postgres-roles user-processes))
     (start #~(make-forkexec-constructor
               (list #$(file-append package "/bin/app.py"))
               #:environment-variables (list #$@environment-variables)
               #:group #$(asahi-stats-configuration-group config)
               #:log-file #$(asahi-stats-configuration-log-file config)
               #:user #$(asahi-stats-configuration-user config)))
     (stop #~(make-kill-destructor)))))

(define (asahi-stats-shepherd-service config)
  "Return the services for the Asahi Installer Stats services for CONFIG."
  (let ((package (asahi-stats-configuration-package config)))
    (list (asahi-stats-schema-service config)
          (asahi-stats-server-service config))))

(define (asahi-stats-postgresql-role config)
  "Return the PostgreSQL role for the Asahi Installer Stats for CONFIG."
  (list (postgresql-role
         (name (asahi-stats-configuration-user config))
         (create-database? #t))))

(define asahi-stats-service-type
  (service-type
   (name 'asahi-stats)
   (description "Asahi Linux Install Stats")
   (extensions
    (list (service-extension
           account-service-type
           asahi-stats-account)
          (service-extension
           profile-service-type
           (compose list asahi-stats-configuration-package))
          (service-extension
           postgresql-service-type
           (const #t))
          (service-extension
           postgresql-role-service-type
           asahi-stats-postgresql-role)
          (service-extension
           shepherd-root-service-type
           asahi-stats-shepherd-service)))
   (default-value (asahi-stats-configuration))))

(define-module (asahi guix packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages databases)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public asahi-stats
  (let ((commit "0f77a2f533791b497d68fba0ef569c5d7e7e4df4")
        (revision "0"))
    (package
      (name "asahi-stats")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AsahiLinux/asahistats")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "18q6gakysnkhlspmf5kaczy2hw66f7qs0zcg0dcaffh9mplr1w5h"))))
      (build-system python-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-source
              (lambda* _
                (substitute* "app.py"
                  (("DBINFO = .*")
                   (string-append
                    "import os\n"
                    "USER = os.environ.get('ASAHI_STATS_USER', 'asahistats')\n"
                    "DBNAME = os.environ.get('ASAHI_STATS_DB', 'asahistats')\n"
                    "PORT = int(os.environ.get('ASAHI_STATS_PORT', '8000'))\n"
                    "DBINFO = f'dbname={DBNAME} user={USER}'\n\n"))
                  (("8000, app") "PORT, app")
                  (("8000\\.\\.\\.\\\"") "\" + f'{PORT}...'"))
                (substitute* "schema.sql"
                  (("CREATE TABLE") "CREATE TABLE IF NOT EXISTS"))))
            (delete 'build)
            (delete 'check)
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref %outputs "out")))
                  (chmod "app.py" #o755)
                  (install-file "app.py"
                                (string-append out "/bin"))
                  (install-file "schema.sql"
                                (string-append out "/share/sql"))))))))
      (home-page "https://github.com/AsahiLinux/asahistats")
      (propagated-inputs (list python-psycopg2))
      (synopsis "Asahi Linux install statistics server")
      (description "The package provides a WSGI server to take install reports and stash
them in a Postgres database as-is.")
      (license license:expat))))

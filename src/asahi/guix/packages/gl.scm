(define-module (asahi guix packages gl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages llvm)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public asahi-mesa
  (let ((commit "6d00b02b727f89a324e7e3dcb34e6c74ef0848b3"))
    (package/inherit mesa
      (name "asahi-mesa")
      (version (git-version "20240727" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/asahi/mesa.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ii6h5sjf2wyy2xqhymdswi37l0r7wqs904p9p50h9vhwy4ncy2x"))))
      (arguments
       (substitute-keyword-arguments (package-arguments mesa)
         ((#:configure-flags flags)
          `(list "-Db_lto=false"
                 "-Db_ndebug=true"
                 "-Dbuild-tests=true"
                 "-Ddri3=enabled"
                 "-Degl=enabled"
                 "-Dgallium-drivers=asahi,kmsro,svga,swrast,virgl,zink"
                 "-Dgallium-extra-hud=true"
                 "-Dgallium-opencl=disabled"
                 "-Dgallium-rusticl=false"
                 "-Dgallium-va=disabled"
                 "-Dgallium-vdpau=disabled"
                 "-Dgallium-xa=enabled"
                 "-Dgbm=enabled"
                 "-Dgles1=enabled"
                 "-Dgles2=enabled"
                 "-Dglx=dri"
                 "-Dlibunwind=disabled"
                 "-Dllvm=enabled"
                 "-Dlmsensors=enabled"
                 "-Dmicrosoft-clc=disabled"
                 "-Dosmesa=true"
                 "-Dplatforms=x11,wayland"
                 "-Dshared-glapi=enabled"
                 "-Dvalgrind=enabled"
                 "-Dvideo-codecs=all"
                 "-Dvulkan-drivers=swrast"
                 "-Dvulkan-layers=device-select,overlay"
                 "-Dzstd=enabled"))
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (delete 'set-layer-path-in-manifests)))))
      (inputs
       (modify-inputs (package-inputs mesa)
         (prepend `(,lm-sensors "lib") asahi-libclc clang-18 libressl valgrind)
         (replace "llvm" llvm-18)
         (replace "llvm-for-mesa" llvm-18))))))

(define-public asahi-mesa-headers
  (package/inherit mesa-headers
    (name "asahi-mesa-headers")
    (version (package-version asahi-mesa))
    (source (package-source asahi-mesa))))

(define replace-mesa
  (package-input-rewriting/spec
   `(("mesa" . ,(const asahi-mesa)))))

(define-public asahi-mesa-utils
  ;; TODO: glxinfo and friends are not compiled
  (package
    (inherit (replace-mesa mesa-utils))
    (name "asahi-mesa-utils")
    (version "9.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mesa3d.org/demos"
                           "/mesa-demos-" version ".tar.xz"))
       (sha256 (base32 "0ss9xpqykwfzkhr55nbfml61dsxz4dgpl9fxxgvil1bvdb9a6iih"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (invoke "ninja" "install"))))))))

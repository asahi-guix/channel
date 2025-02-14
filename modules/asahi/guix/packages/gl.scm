(define-module (asahi guix packages gl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public asahi-mesa
  (let ((commit "819a5d4523f2c89e05f8d7f5a16ffb3b1f288f4b"))
    (package/inherit mesa
      (name "asahi-mesa")
      (version (git-version "20241211" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/asahi/mesa.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19h39p5f1676pzag1a4hv8351pcrgdmyjwmfa3hm4s9bv7z0qbip"))))
      (arguments
       (substitute-keyword-arguments (package-arguments mesa)
         ((#:configure-flags flags)
          `(list "-Db_lto=false"
                 "-Db_ndebug=true"
                 "-Dbuild-tests=true"
                 ;; "-Ddri3=enabled"
                 "-Degl=enabled"
                 "-Dgallium-drivers=asahi,svga,swrast,virgl,zink"
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
                 "-Dvalgrind=disabled"
                 "-Dvideo-codecs=all"
                 "-Dvulkan-drivers=asahi,swrast"
                 "-Dvulkan-layers=device-select,overlay"
                 "-Dzstd=enabled"))))
      (inputs
       (modify-inputs (package-inputs mesa)
         (prepend `(,lm-sensors "lib"))
         (replace "wayland-protocols" wayland-protocols-next))))))

(define-public asahi-mesa-headers
  (package/inherit mesa-headers
    (name "asahi-mesa-headers")
    (version (package-version asahi-mesa))
    (source (package-source asahi-mesa))))

(define-public replace-mesa
  (package-input-rewriting/spec
   `(("mesa" . ,(const asahi-mesa))
     ("mesa-headers" . ,(const asahi-mesa-headers)))))

(define-public asahi-mesa-utils
  (package
    (inherit mesa-utils)
    (name "asahi-mesa-utils")
    (inputs
     (list asahi-mesa
           (replace-mesa freeglut)
           (replace-mesa glew)))))

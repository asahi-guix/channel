bin_SCRIPTS =

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = \
  pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -L src -o "$@" "$<"

NOCOMP_SOURCES = modules/asahi/guix/files/authorized-keys/substitutes.asahi-guix.org.pub \
	modules/asahi/guix/files/bootlogo.svg \
	modules/asahi/guix/packages/defconfig \
	tests/asahi/guix/installer/alx.sh

SOURCES = modules/asahi/guix/build/block-devices.scm \
	modules/asahi/guix/build/bootloader/m1n1.scm \
	modules/asahi/guix/build/firmware.scm \
	modules/asahi/guix/build/modules.scm \
	modules/asahi/guix/build/sfdisk.scm \
	modules/asahi/guix/build/utils.scm \
	modules/asahi/guix/channels.scm \
	modules/asahi/guix/config.scm \
	modules/asahi/guix/home/config.scm \
	modules/asahi/guix/home/services/sound.scm \
	modules/asahi/guix/images/base.scm \
	modules/asahi/guix/images/gnome.scm \
	modules/asahi/guix/images/installer.scm \
	modules/asahi/guix/images/plasma.scm \
	modules/asahi/guix/images/sway.scm \
	modules/asahi/guix/initrd.scm \
	modules/asahi/guix/installer/data.scm \
	modules/asahi/guix/installer/os.scm \
	modules/asahi/guix/installer/package.scm \
	modules/asahi/guix/installer/partition.scm \
	modules/asahi/guix/installer/script.scm \
	modules/asahi/guix/manifests.scm \
	modules/asahi/guix/packages.scm \
	modules/asahi/guix/packages/audio.scm \
	modules/asahi/guix/packages/bootloader.scm \
	modules/asahi/guix/packages/crates-io.scm \
	modules/asahi/guix/packages/display-managers.scm \
	modules/asahi/guix/packages/firmware.scm \
	modules/asahi/guix/packages/gl.scm \
	modules/asahi/guix/packages/installer.scm \
	modules/asahi/guix/packages/installer.scm \
	modules/asahi/guix/packages/linux.scm \
	modules/asahi/guix/packages/misc.scm \
	modules/asahi/guix/packages/music.scm \
	modules/asahi/guix/packages/pulseaudio.scm \
	modules/asahi/guix/packages/wm.scm \
	modules/asahi/guix/packages/xorg.scm \
	modules/asahi/guix/search-paths.scm \
	modules/asahi/guix/services/console-font.scm \
	modules/asahi/guix/services/firmware.scm \
	modules/asahi/guix/services/sound.scm \
	modules/asahi/guix/services/udev.scm \
	modules/asahi/guix/systems/base.scm \
	modules/asahi/guix/systems/desktop.scm \
	modules/asahi/guix/systems/gnome.scm \
	modules/asahi/guix/systems/install.scm \
	modules/asahi/guix/systems/plasma.scm \
	modules/asahi/guix/systems/sway.scm \
	modules/asahi/guix/transformations.scm \
	modules/asahi/guix/udev.scm \
	modules/gnu/bootloader/m1n1.scm \
	modules/gnu/system/images/asahi/installer.scm

TESTS = \
 tests/asahi/guix/build/block-devices.scm \
 tests/asahi/guix/installer/data.scm \
 tests/asahi/guix/installer/script.scm \
 tests/asahi/guix/build/sfdisk.scm \
 tests/asahi/guix/build/utils.scm

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main -L src     \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

info_TEXINFOS = doc/ashai-guix.texi
dvi: # Don't build dvi docs

EXTRA_DIST += NOTES.org \
              README.org \
              README \
              COPYING \
              HACKING \
              .guix-authorizations \
              .guix-channel \
              .gitignore \
	      share/systems/gnome.tmpl \
	      share/systems/plasma.tmpl \
	      share/systems/sway.tmpl \
              share/systems/base.tmpl \
              .github/workflows/aarch64-linux-gnu.yml \
              .github/workflows/x86_64-linux-gnu.yml \
              .github/FUNDING.yml \
              build-aux/test-driver.scm \
              $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)

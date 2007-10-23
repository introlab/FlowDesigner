## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

#EXTRA_DIST = BUGS INSTALL.generic
#SUBDIRS = data-flow audio_blocks VQ HMM NNet tools vflow video_blocks

bin_SCRIPTS = flowdesigner-config

EXTRA_DIST = FlowDesigner.spec FlowDesigner.spec.in nflow.dtd FlowDesigner.pc.in CMakeLists.txt COPYING.LGPL license.txt

#Fools KDevelop into finding all the files
if COND_OPT
SUBDIRS = data-flow @modules@ tools examples
else
SUBDIRS = data-flow audio_blocks VQ HMM NNet qtflow FuzzyEngine effects lapackflow octave vflow tools examples
endif

DIST_SUBDIRS = data-flow audio_blocks VQ HMM NNet FuzzyEngine effects lapackflow octave vflow qtflow tools examples

rpm: dist
	rpmbuild -ta ${PACKAGE}-${VERSION}.tar.gz

DEB_PACKAGE=$(PWD)/deb_package

deb_package: all
	mkdir -p ${DEB_PACKAGE}
	make prefix=${DEB_PACKAGE}/usr \
        exec_prefix=${DEB_PACKAGE}/usr \
        bindir=${DEB_PACKAGE}/usr/bin \
        sbindir=${DEB_PACKAGE}/usr/sbin \
        sysconfdir=${DEB_PACKAGE}/usr/etc \
        datadir=${DEB_PACKAGE}/usr/share \
        includedir=${DEB_PACKAGE}/usr/include \
        libdir=${DEB_PACKAGE}/usr/lib \
        libexecdir=${DEB_PACKAGE}/usr/libexec \
        localstatedir=${DEB_PACKAGE}/usr/var \
        sharedstatedir=${DEB_PACKAGE}/usr/com \
        mandir=${DEB_PACKAGE}/usr/share/man \
        infodir=${DEB_PACKAGE}/usr/share/info \
   	install 

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = FlowDesigner.pc

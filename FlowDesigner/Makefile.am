## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

#EXTRA_DIST = BUGS INSTALL.generic
#SUBDIRS = data-flow audio_blocks VQ HMM NNet tools vflow video_blocks

bin_SCRIPTS = overflow-config

EXTRA_DIST = Overflow.spec nflow.dtd

SUBDIRS = data-flow @modules@ tools examples

rpm: dist
	rpmbuild -ta ${PACKAGE}-${VERSION}.tar.gz

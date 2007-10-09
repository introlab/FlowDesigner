## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

#EXTRA_DIST = BUGS INSTALL.generic
#SUBDIRS = data-flow audio_blocks VQ HMM NNet tools vflow video_blocks

bin_SCRIPTS = flowdesigner-config

EXTRA_DIST = FlowDesigner.spec FlowDesigner.spec.in nflow.dtd FlowDesigner.pc.in CMakeLists.txt

#Fools KDevelop into finding all the files
if COND_OPT
SUBDIRS = data-flow @modules@ tools examples
else
SUBDIRS = data-flow audio_blocks VQ HMM NNet qtflow FuzzyEngine effects lapackflow octave vflow tools examples
endif

DIST_SUBDIRS = data-flow audio_blocks VQ HMM NNet FuzzyEngine effects lapackflow octave vflow qtflow tools examples

rpm: dist
	rpmbuild -ta ${PACKAGE}-${VERSION}.tar.gz
   

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = FlowDesigner.pc

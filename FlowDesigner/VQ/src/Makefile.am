## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.13 2000/07/20 06:03:27 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libvq.la

install:
	(cd $(prefix)/lib; rm -f vq.tlb; ln -s libvq.so vq.tlb)
	mkdir -p $(prefix)/toolbox/vq
	$(top_srcdir)/../tools/src/scan_toolbox.sh $(top_srcdir)/../tools/src/info2def.pl  $(top_srcdir)/src $(prefix)/toolbox/vq

# Sources for compilation in the library
libvq_la_SOURCES = kmeans.cc \
	Cell.cc \
	MMITrain.cc \
	VQuantize.cc \
	MMIScore.cc \
	VQTrain.cc \
	VQClass.cc \
	CMCalc.cc \
	CMTrain.cc \
	CodebookMap.cc \
	VQCloseness.cc \
	VQWeightMeans.cc \
	msvq.cc \
	MSVQTrain.cc

#libvq_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libvq_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

noinst_PROGRAMS = test testmmi
test_SOURCES = test.cc
testmmi_SOURCES = testmmi.cc

LDADD = $(top_srcdir)/src/libvq.la \
        $(top_srcdir)/../data-flow/src/libnetwork.la

#ifdef FREEBSD
FREEBSD_FLAGS = -pthread
#endif

LDFLAGS = $(FREEBSD_FLAGS)


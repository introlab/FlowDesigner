override includedir=${prefix}/lib/flowdesigner/toolbox/VQ
override datadir=${prefix}/lib/flowdesigner/toolbox/VQ
override libdir=${prefix}/lib/flowdesigner/toolbox/VQ
## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.32 2004/01/13 22:12:41 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libvq.la

install-data-local:
	mkdir -p $(datadir)
	(cd $(libdir); rm -f vq.tlb; ln -s libvq.so vq.tlb)
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h > $(datadir)/vq.def)

# Sources for compilation in the library
libvq_la_SOURCES = kmeans.cc \
	Cell.cc \
	MMITrain.cc \
	VQuantize.cc \
	VQuantizeDiff.cc \
	MMIScore.cc \
	VQTrain.cc \
	VQClass.cc \
	CMCalc.cc \
	CMTrain.cc \
	CodebookMap.cc \
	VQCloseness.cc \
	VQWeightMeans.cc \
	msvq.cc \
	MSVQTrain.cc \
	RBF.cc \
	RBFTrain.cc \
	FMapCalc.cc \
	FMapTrain.cc \
	FeatureMap.cc

#libvq_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libvq_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

LDADD = $(top_srcdir)/src/libvq.la \
	$(top_srcdir)/../data-flow/src/libflow.la



override includedir=${prefix}/lib/flowdesigner/toolbox/VQ
override datadir=${prefix}/lib/flowdesigner/toolbox/VQ
override libdir=${prefix}/lib/flowdesigner/toolbox/VQ
## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.34 2005/11/01 21:45:33 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libvq.la



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

install-data-local:
	mkdir -p $(datadir)
if WITH_CYGWIN
	(cd $(libdir); rm -f vq.tlb; ln -s libvq.dll.a vq.tlb)	
else
	(cd $(libdir); rm -f vq.tlb; ln -s libvq.so vq.tlb)
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(libvq_la_SOURCES) > $(datadir)/vq.def)



libvq_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS) $(top_srcdir)/../data-flow/src/libflow.la

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

LDADD = $(top_srcdir)/src/libvq.la \
	$(top_srcdir)/../data-flow/src/libflow.la



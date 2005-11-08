## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.46 2005/11/07 17:59:59 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

override includedir=${prefix}/lib/flowdesigner/toolbox/NNet
override datadir=${prefix}/lib/flowdesigner/toolbox/NNet
override libdir=${prefix}/lib/flowdesigner/toolbox/NNet
override bindir=${prefix}/lib/flowdesigner/toolbox/NNet

lib_LTLIBRARIES = libnnet.la

# Sources for compilation in the library
libnnet_la_SOURCES = FFLayer.cc FFNet.cc \
	NNetExec.cc NNetTrainDBD.cc \
	NNetSet.cc NNetSetCalc.cc \
	NNetSetTrainDBD.cc \
	NNetInit.cc \
	NNetSetInit.cc \
	NNetSetChooseBest.cc \
	NNetTrainQProp.cc \
	NNetTrainSCG.cc \
	TrainingAlgo.cc \
	NNetTrainWeightDBD.cc


install-data-local:
	mkdir -p $(datadir)
if WITH_CYGWIN
	(cd $(libdir); rm -f NNet.tlb; \
	ln -s $(prefix)/lib/flowdesigner/toolbox/bin/cygnnet-$(FLOWDESIGNER_MAJOR_VERSION)-$(FLOWDESIGNER_MINOR_VERSION).dll NNet.tlb)
else
	(cd $(libdir); rm -f NNet.tlb; ln -s libnnet.so NNet.tlb)
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(libnnet_la_SOURCES) > $(datadir)/NNet.def)

libnnet_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS) $(top_srcdir)/../data-flow/src/libflow.la

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

LDADD = $(top_srcdir)/src/libnnet.la \
        $(top_srcdir)/../data-flow/src/libflow.la

LDFLAGS = 

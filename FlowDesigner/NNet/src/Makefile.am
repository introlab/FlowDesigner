## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.42 2004/01/13 22:12:41 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

override includedir=${prefix}/lib/flowdesigner/toolbox/NNet
override datadir=${prefix}/lib/flowdesigner/toolbox/NNet
override libdir=${prefix}/lib/flowdesigner/toolbox/NNet

lib_LTLIBRARIES = libnnet.la

install-data-local:
	(cd $(libdir); rm -f NNet.tlb; ln -s libnnet.so NNet.tlb)
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h > $(datadir)/NNet.def)

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

#libvq_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libnnet_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

LDADD = $(top_srcdir)/src/libnnet.la \
        $(top_srcdir)/../data-flow/src/libflow.la

LDFLAGS = 

## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.35 2001/07/07 07:53:13 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libnnet.la

install:
	(cd $(libdir); rm -f nnet.tlb; ln -s libnnet.so nnet.tlb)
	mkdir -p $(datadir)/toolbox/nnet
	perl $(top_srcdir)/../tools/src/info2def.pl $(top_srcdir)/src/*.cc > $(datadir)/toolbox/nnet/nnet.def

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

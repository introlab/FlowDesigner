## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.28 2001/02/28 18:29:37 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libnnet.la

install:
	(cd $(prefix)/lib; rm -f nnet.tlb; ln -s libnnet.so nnet.tlb)
	mkdir -p $(prefix)/toolbox/nnet
	perl $(top_srcdir)/../tools/src/info2def.pl $(top_srcdir)/src/*.cc > $(prefix)/toolbox/nnet/nnet.def

# Sources for compilation in the library
libnnet_la_SOURCES = FFLayer.cc FFNet.cc \
	NNetExec.cc NNetTrainDBD.cc \
	NNetSet.cc NNetSetCalc.cc \
	NNetSetTrainDBD.cc \
	NNetInit.cc \
	NNetSetInit.cc \
	NNetSetChooseBest.cc

#libvq_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libnnet_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include \
	$(GNOME_XML_INCLUDE)

noinst_PROGRAMS = test 
test_SOURCES = test.cc

LDADD = $(top_srcdir)/src/libnnet.la \
        $(top_srcdir)/../data-flow/src/libflow.la

#ifdef FREEBSD
FREEBSD_FLAGS = -pthread
#endif

LDFLAGS = $(FREEBSD_FLAGS) $(GNOME_XML_LIB)

## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.17 2000/09/26 04:23:23 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libhmm.la

install:
	(cd $(prefix)/lib; rm -f hmm.tlb; ln -s libhmm.so hmm.tlb)
	mkdir -p $(prefix)/toolbox/hmm
	perl $(top_srcdir)/../tools/src/info2def.pl $(top_srcdir)/src/*.cc > $(prefix)/toolbox/hmm/hmm.def

# Sources for compilation in the library
libhmm_la_SOURCES =	gmm.cc covariance.cc gaussian.cc \
	audioinfo.cc acoustic_model.cc covariance_set.cc \
	mean_set.cc gaussian_set.cc gmm_set.cc mean.cc \
	GMMScore.cc GMMTrain.cc TransMatrix.cc MarkovProb.cc

#libhmm_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libhmm_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include \
	$(GNOME_XML_INCLUDE)

noinst_PROGRAMS = test
test_SOURCES = test.cc

LDADD = $(top_srcdir)/src/libhmm.la \
	$(top_srcdir)/../data-flow/src/libflow.la $(GNOME_XML_LIB)

#ifdef FREEBSD
FREEBSD_FLAGS = -pthread
#endif

LDFLAGS = $(FREEBSD_FLAGS)

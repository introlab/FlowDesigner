override includedir=${prefix}/include/flowdesigner
override datadir=${prefix}/share/flowdesigner
override libdir=${prefix}/share/flowdesigner/lib

## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.31 2003/11/13 02:54:27 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libhmm.la

install-data-local:
	mkdir -p $(datadir)/toolbox/hmm
	(cd $(libdir); rm -f hmm.tlb; ln -s libhmm.so hmm.tlb)
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h > $(datadir)/toolbox/hmm/hmm.def)

# Sources for compilation in the library
libhmm_la_SOURCES =	gmm.cc covariance.cc gaussian.cc \
	audioinfo.cc acoustic_model.cc covariance_set.cc \
	mean_set.cc gaussian_set.cc gmm_set.cc mean.cc \
	GMMScore.cc GMMTrain.cc TransMatrix.cc MarkovProb.cc AdaptMAP.cc \
	DiagGMM.cc MakeDiagGMM.cc DiagGMMScore.cc DiagGMMSetScore.cc

#libhmm_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libhmm_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

LDADD = $(top_srcdir)/src/libhmm.la \
	$(top_srcdir)/../data-flow/src/libflow.la


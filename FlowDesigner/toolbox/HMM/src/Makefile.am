override includedir=${prefix}/lib/flowdesigner/toolbox/HMM
override datadir=${prefix}/lib/flowdesigner/toolbox/HMM
override libdir=${prefix}/lib/flowdesigner/toolbox/HMM
override bindir=${prefix}/lib/flowdesigner/toolbox/HMM
## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.36 2005/11/07 17:59:59 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libhmm.la



# Sources for compilation in the library
libhmm_la_SOURCES =	gmm.cc covariance.cc gaussian.cc \
	audioinfo.cc acoustic_model.cc covariance_set.cc \
	mean_set.cc gaussian_set.cc gmm_set.cc mean.cc \
	GMMScore.cc GMMTrain.cc TransMatrix.cc MarkovProb.cc AdaptMAP.cc \
	DiagGMM.cc MakeDiagGMM.cc DiagGMMScore.cc DiagGMMSetScore.cc DiagGMMTrain.cc


install-data-local:
	mkdir -p $(datadir)
if WITH_CYGWIN
	(cd $(libdir); rm -f hmm.tlb; \
	 ln -s $(prefix)/lib/flowdesigner/toolbox/bin/cyghmm-$(FLOWDESIGNER_MAJOR_VERSION)-$(FLOWDESIGNER_MINOR_VERSION).dll hmm.tlb)	
else
	(cd $(libdir); rm -f hmm.tlb; ln -s libhmm.so hmm.tlb)
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(libhmm_la_SOURCES) > $(datadir)/hmm.def)



libhmm_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS) $(top_srcdir)/../data-flow/src/libflow.la

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

LDADD = $(top_srcdir)/src/libhmm.la \
	$(top_srcdir)/../data-flow/src/libflow.la


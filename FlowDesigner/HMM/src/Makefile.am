## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.5 1999/09/18 23:14:41 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libhmm.la

# Sources for compilation in the library
libhmm_la_SOURCES =	gmm.cc covariance.cc gaussian.cc \
	audioinfo.cc accoustic_model.cc covariance_set.cc \
	mean_set.cc gaussian_set.cc gmm_set.cc

#libhmm_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libhmm_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/HMM/include -I$(top_srcdir)/data-flow/include

noinst_PROGRAMS = test
test_SOURCES = test.cc

LDADD = $(top_srcdir)/HMM/src/libhmm.la \
	$(top_srcdir)/data-flow/src/libnetwork.la

## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.2 1999/08/13 17:38:43 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libhmm.la

# Sources for compilation in the library
libhmm_la_SOURCES =	gmm.cc covariance.cc gaussian.cc

#libhmm_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libhmm_la_LDFLAGS = -release $(LT_RELEASE) @LIBS@

INCLUDES = -I$(top_srcdir)/HMM/include -I$(top_srcdir)/data-flow/include

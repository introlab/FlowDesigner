## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.7 2000/03/01 21:30:59 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libnnet.la

# Sources for compilation in the library
libnnet_la_SOURCES = FFLayer.cc FFNet.cc NNetTrain.cc NNetExec.cc NNetNew.cc NNetTrainCG.cc

#libvq_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libnnet_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

noinst_PROGRAMS = test 
test_SOURCES = test.cc

LDADD = $(top_srcdir)/src/libnnet.la \
        $(top_srcdir)/../data-flow/src/libnetwork.la \

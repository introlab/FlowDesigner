## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.1 2000/01/27 01:00:50 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libnnet.la

# Sources for compilation in the library
libnnet_la_SOURCES = FFLayer.cc FFNet.cc NNetTrain.cc

#libvq_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libnnet_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/NNet/include -I$(top_srcdir)/data-flow/include -I$(top_srcdir)/audio_blocks/include

noinst_PROGRAMS = test 
test_SOURCES = test.cc

LDADD = $(top_srcdir)/NNet/src/libnnet.la \
	$(top_srcdir)/audio_blocks/src/libablocks.la \
        $(top_srcdir)/data-flow/src/libnetwork.la \
	$(LIB_FFTW)


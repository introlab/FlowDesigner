## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.4 2000/02/23 04:53:41 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libvq.la

# Sources for compilation in the library
libvq_la_SOURCES = kmeans.cc \
	Cell.cc \
	MMITrain.cc \
	VQuantize.cc \
	MMIScore.cc \
	VQTrain.cc

#libvq_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libvq_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

noinst_PROGRAMS = test testmmi
test_SOURCES = test.cc
testmmi_SOURCES = testmmi.cc

LDADD = $(top_srcdir)/src/libvq.la \
        $(top_srcdir)/../data-flow/src/libnetwork.la


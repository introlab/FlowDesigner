## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.8 1999/08/18 17:40:34 dletour Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libablocks.la

# Sources for compilation in the library
libablocks_la_SOURCES =	DLFrameUnaryOperation.cc \
        FrameUnaryOperation.cc \
        FrameBinaryOperation.cc \
        FrameOperation.cc \
        Concatenate.cc \
        FrameAccumulator.cc \
        AudioFile.cc \
        GMMTrain.cc \
        GMMScore.cc \
        GCMS.cc \
	MMIAccumulator.cc \
	MMITrain.cc \
	MMIScore.cc \
	Histogram.cc


#libablocks_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libablocks_la_LDFLAGS = -release $(LT_RELEASE) @LIBS@

# Flags for SGI CC:
#libablocks_la_CXXFLAGS = -n32

INCLUDES = -I$(top_srcdir)/HMM/include -I$(top_srcdir)/data-flow/include -I$(top_srcdir)/audio_blocks/include -I$(top_srcdir)/MMI/include

bin_PROGRAMS = AudioNetwork

AudioNetwork_SOURCES = AudioNetwork.cc 

LDFLAGS=$(FFTW_LDFLAGS)

LDADD = $(top_srcdir)/audio_blocks/src/libablocks.la \
	$(top_srcdir)/HMM/src/libhmm.la \
	$(top_srcdir)/MMI/src/libmmi.la \
	$(top_srcdir)/data-flow/src/libnetwork.la \
	$(LIB_FFTW)


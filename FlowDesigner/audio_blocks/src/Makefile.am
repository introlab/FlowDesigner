## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.9 1999/09/24 02:50:09 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libablocks.la 
#\
#	libAudioFile.la \
#	libConcatenate.la \
#	libDLFrameUnaryOperation.la \
#	libFrameAccumulator.la

#libAudioFile_la_SOURCES = AudioFile.cc
#libAudioFile_la_LDFLAGS = -release $(LT_RELEASE)

#libConcatenate_la_SOURCES = Concatenate.cc
#libConcatenate_la_LDFLAGS = -release $(LT_RELEASE)

#libDLFrameUnaryOperation_la_SOURCES = DLFrameUnaryOperation.cc
#libDLFrameUnaryOperation_la_LDFLAGS = -release $(LT_RELEASE)

#libFrameAccumulator_la_SOURCES = FrameAccumulator.cc
#libFrameAccumulator_la_LDFLAGS = -release $(LT_RELEASE)

# Sources for compilation in the library
libablocks_la_SOURCES =	DLFrameUnaryOperation.cc \
        FrameUnaryOperation.cc \
        FrameBinaryOperation.cc \
        FrameOperation.cc \
        GMMTrain.cc \
        GMMScore.cc \
        GCMS.cc \
	MMIAccumulator.cc \
	MMITrain.cc \
	MMIScore.cc \
	Histogram.cc \
	PitchDecision.cc \
	Sync.cc \
	GenderID.cc \
	FFT.cc \
	PS.cc \
	Mel.cc \
	Window.cc \
	DCT.cc \
	Select.cc \
	TimeFilter.cc \
	Log.cc \
	FrameAccumulator.cc \
	AudioFile.cc \
	Concatenate.cc \
	Entropy.cc \
	PCMConvert.cc \
	SaveFrame.cc \
	OverlapAndAdd.cc \
	Gain.cc \
	AllZero.cc \
	AllPole.cc \
	LPC.cc \
	ZeroPhase.cc \
	MixLFHF.cc


#libablocks_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libablocks_la_LDFLAGS = -release $(LT_RELEASE)

# Flags for SGI CC:
#libablocks_la_CXXFLAGS = -n32

INCLUDES = -I$(top_srcdir)/HMM/include -I$(top_srcdir)/data-flow/include -I$(top_srcdir)/audio_blocks/include -I$(top_srcdir)/MMI/include $(FFTW_INCLUDES)

bin_PROGRAMS = AudioNetwork

AudioNetwork_SOURCES = AudioNetwork.cc 

LDFLAGS=$(FFTW_LDFLAGS)

LDADD = $(top_srcdir)/audio_blocks/src/libablocks.la \
	$(top_srcdir)/HMM/src/libhmm.la \
	$(top_srcdir)/MMI/src/libmmi.la \
	$(top_srcdir)/data-flow/src/libnetwork.la \
	$(LIB_FFTW)


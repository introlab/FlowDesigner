## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.23 2000/01/26 00:12:20 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libablocks.la 

libablocks_la_SOURCES =	DLFrameUnaryOperation.cc \
        FrameUnaryOperation.cc \
        FrameBinaryOperation.cc \
        FrameOperation.cc \
        GCMS.cc \
	MMIAccumulator.cc \
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
	Exp.cc \
	Abs.cc \
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
	CAllZero.cc \
	CAllPole.cc \
	UpSample.cc \
	DownSample.cc \
	Add.cc \
	FIR.cc \
	Div.cc \
	LPC2PS.cc \
	LPC.cc \
	PS2LPC.cc \
	Pitch.cc \
	LPFilter.cc \
	Concat.cc \
	MFCC.cc \
	Sound.cc \
	FDSaveFrame.cc

libablocks_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/data-flow/include -I$(top_srcdir)/audio_blocks/include $(FFTW_INCLUDES)

LDADD = $(top_srcdir)/data-flow/src/libnetwork.la \
	$(LIB_FFTW)

#bin_PROGRAMS = AudioNetwork

#AudioNetwork_SOURCES = AudioNetwork.cc 

#LDFLAGS=$(FFTW_LDFLAGS)

#LDADD = $(top_srcdir)/audio_blocks/src/libablocks.la \
#	$(top_srcdir)/HMM/src/libhmm.la \
#	$(top_srcdir)/VQ/src/libvq.la \
#	$(top_srcdir)/data-flow/src/libnetwork.la \
#	$(LIB_FFTW)


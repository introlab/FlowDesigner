## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.37 2000/03/26 02:13:05 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libablocks.la 

libablocks_la_SOURCES =	\
        GCMS.cc \
	MMIAccumulator.cc \
	Histogram.cc \
	PitchDecision.cc \
	Sync.cc \
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
	Entropy.cc \
	PCMConvert.cc \
	SaveFrame.cc \
	OverlapAndAdd.cc \
	Gain.cc \
	CAllPole.cc \
	UpSample.cc \
	DownSample.cc \
	Add.cc \
	FIR.cc \
	Div.cc \
	LPC2PS.cc \
	LPC.cc \
	PS2LPC.cc \
	LPFilter.cc \
	Concat.cc \
	MFCC.cc \
	Sound.cc \
	FDSaveFrame.cc \
	AudioStream.cc \
	TimeAutocorr.cc \
	HistoVect.cc \
	Dist.cc \
	NormalizeAutocorr.cc \
	Max.cc \
	IDCT.cc \
	CMS.cc \
	LTP.cc \
	LTF.cc \
	ILTF.cc

libablocks_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include $(FFTW_INCLUDES)

LDADD = $(top_srcdir)/../data-flow/src/libnetwork.la \
	$(LIB_FFTW)



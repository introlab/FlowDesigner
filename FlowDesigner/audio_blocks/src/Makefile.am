## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.57 2000/09/02 05:18:20 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libablocks.la 

install:
	(cd $(prefix)/lib; rm -f audio_blocks.tlb; ln -s libablocks.so audio_blocks.tlb)
	mkdir -p $(prefix)/toolbox/audio_blocks
	perl $(top_srcdir)/../tools/src/info2def.pl $(top_srcdir)/src/*.cc > $(prefix)/toolbox/audio_blocks/audio_blocks.def

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
	ILTF.cc \
	CGain.cc \
	Autocor.cc \
	Overlap.cc \
	Amplitude.cc \
	Mul.cc \
	Sub.cc \
	MergeChannels.cc \
	SeparChannels.cc \
	Sqrt.cc \
	InvalidPad.cc \
	Poly.cc \
	mdct.c \
	window.c \
	psy.c \
	MDCT.cc \
	VorbisPsycho.cc \
	Pow.cc \
	TimeEntropy.cc \
	SmoothAdd.cc \
	FFTWrap.cc \
	Concatenate.cc \
	Normalize.cc \
	Index2Vector.cc

libablocks_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include $(FFTW_INCLUDES)

LDADD = $(top_srcdir)/../data-flow/src/libnetwork.la \
	$(LIB_FFTW)



override includedir=${prefix}/lib/flowdesigner/toolbox/audio_blocks
override datadir=${prefix}/lib/flowdesigner/toolbox/audio_blocks
override libdir=${prefix}/lib/flowdesigner/toolbox/audio_blocks

## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.101 2004/02/06 15:42:31 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libablocks.la 

install-data-local:
	mkdir -p $(datadir)
	(cd $(libdir); rm -f audio_blocks.tlb; ln -s libablocks.so audio_blocks.tlb)
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h > $(datadir)/audio_blocks.def)

libablocks_la_SOURCES =	\
	GCMS.cc \
	GCMS2.cc \
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
	Entropy.cc \
	OverlapAndAdd.cc \
	Gain.cc \
	IIR.cc \
	UpSample.cc \
	DownSample.cc \
	FIR.cc \
	LPC2PS.cc \
	LPC.cc \
	PS2LPC.cc \
	LPFilter.cc \
	MFCC.cc \
	Sound.cc \
	FDSaveFrame.cc \
	AudioStream.cc \
	TimeAutocorr.cc \
	HistoVect.cc \
	Dist.cc \
	ArgMax.cc \
	IDCT.cc \
	CMS.cc \
	LTP.cc \
	LTF.cc \
	ILTF.cc \
	CGain.cc \
	Autocor.cc \
	Overlap.cc \
	Amplitude.cc \
	MergeChannels.cc \
	SeparChannels.cc \
	Sqrt.cc \
	Poly.cc \
	mdct.c \
	window.c \
	MDCT.cc \
	Pow.cc \
	TimeEntropy.cc \
	SmoothAdd.cc \
	Concatenate.cc \
	Normalize.cc \
	Index2Vector.cc \
	BWExpan.cc \
	TimeMedian.cc \
	FFTWrap.cc \
	NLMS.cc \
	RMS.cc \
	PackFrames.cc \
	RFFT.cc \
	Conj.cc \
	IRFFT.cc \
	SampleDelay.cc \
	FFTFlip.cc \
	Reframe.cc \
	DTMF.cc \
	FrameLabel.cc \
	ZCrossing.cc \
	Floor.cc \
	Noise.cc \
	WriteAudio.cc \
	OffsetMatrix.cc \
	StopRecord.cc \
	FLog.cc \
	CovarianceAccum.cc \
	Sort.cc \
	lsp.c \
	LSP2LPC.cc \
	LPC2LSP.cc

libablocks_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include $(FFTW_INCLUDES)

LDADD = $(top_srcdir)/../data-flow/src/libflow.la $(LIB_FFTW)



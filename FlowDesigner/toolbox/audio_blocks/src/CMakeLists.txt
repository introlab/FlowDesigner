	
SET(AUDIO_SRCS
	GCMS.cc 
	GCMS2.cc 
	Sync.cc
	FFT.cc
	PS.cc
	Mel.cc
	Window.cc
	DCT.cc
	Select.cc
	TimeFilter.cc
	Log.cc
	Exp.cc
	Abs.cc
	Entropy.cc
	OverlapAndAdd.cc
	Gain.cc
	IIR.cc
	UpSample.cc
	DownSample.cc
	FIR.cc
	LPC2PS.cc
	LPC.cc
	PS2LPC.cc
	LPFilter.cc
	MFCC.cc
	FDSaveFrame.cc
	AudioStream.cc
	TimeAutocorr.cc
	HistoVect.cc
	Dist.cc
	ArgMax.cc
	IDCT.cc
	CMS.cc
	LTP.cc
	LTF.cc
	ILTF.cc
	CGain.cc
	Autocor.cc
	Overlap.cc
	Amplitude.cc
	MergeChannels.cc
	SeparChannels.cc
	Sqrt.cc
	Poly.cc
	mdct2.c
	window2.c
	MDCT.cc
	Pow.cc
	TimeEntropy.cc
	SmoothAdd.cc
	Concatenate.cc
	Normalize.cc
	Index2Vector.cc
	BWExpan.cc
	TimeMedian.cc
	FFTWrap.cc
	NLMS.cc
	RMS.cc
	PackFrames.cc
	RFFT.cc
	Conj.cc
	IRFFT.cc
	SampleDelay.cc
	FFTFlip.cc
	Reframe.cc
	DTMF.cc
	FrameLabel.cc
	ZCrossing.cc
	Floor.cc
	Noise.cc
	WriteAudio.cc
	OffsetMatrix.cc
	StopRecord.cc
	FLog.cc
	CovarianceAccum.cc
	Sort.cc
	lsp.c
	LSP2LPC.cc
	LPC2LSP.cc
    Sound.cc
    WavInputSound.cc
)	

IF(UNIX)
    find_package(Alsa)
    IF(LIBALSA_FOUND)
        SET(AUDIO_SRCS ${AUDIO_SRCS}
            ALSASound.cc
        )
    ENDIF(LIBALSA_FOUND)
ENDIF(UNIX)

IF(LIBALSA_FOUND)
    INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	    ../include 
	    ${LIBXML2_INCLUDE} 
        ${LIBALSA_INCLUDE}
    )
ELSE(LIBALSA_FOUND)
    INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	    ../include 
	    ${LIBXML2_INCLUDE}
    )
ENDIF(LIBALSA_FOUND)
	
add_library(libaudio-dynamic SHARED ${AUDIO_SRCS})
	
set_target_properties(libaudio-dynamic PROPERTIES OUTPUT_NAME audio.tlb)

IF(LIBALSA_FOUND)
    target_link_libraries(libaudio-dynamic libflow-dynamic ${LIBXML2_LIBRARIES} ${LIBALSA_LIBRARIES})
ELSE(LIBALSA_FOUND)
    target_link_libraries(libaudio-dynamic libflow-dynamic ${LIBXML2_LIBRARIES})
ENDIF(LIBALSA_FOUND)

install(TARGETS libaudio-dynamic DESTINATION ${FD_TOOLBOX_DIR}/AUDIO)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/toolbox/audio_blocks/src OUTPUT_FILE libaudio.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${AUDIO_SRCS})" )  
INSTALL( FILES libaudio.def DESTINATION ${FD_TOOLBOX_DIR}/AUDIO/)

	
SET(COMMUNICATIONS_SRCS
	iidSource.cc
	DiscreteSource.cc
	ComplexGaussian.cc
	Constellation.cc
	Constellation-dvbt.cc
	ConstellationMapper.cc
	OFDModulator.cc
	LLRComputer.cc
	MLDetector.cc
	OFDDemodulator.cc
	BitErrors.cc
	HardDetector.cc
	MutualInf.cc
	Histogram.cc
	AveragePower.cc
	Interleaver.cc
	SpreadInterleaver.cc
)	

find_package(FFTW3)
IF (FFTW3_FOUND)
    ADD_DEFINITIONS(-DFFTW3_FOUND)
ENDIF (FFTW3_FOUND)

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	../include 
	${LIBXML2_INCLUDE} )
	
add_library(libcommunications-dynamic SHARED ${COMMUNICATIONS_SRCS})
	
set_target_properties(libcommunications-dynamic PROPERTIES OUTPUT_NAME communications.tlb)

target_link_libraries(libcommunications-dynamic libflow-dynamic ${LIBXML2_LIBRARIES} ${FFTW3_LIBRARY})

install(TARGETS libcommunications-dynamic DESTINATION ${FD_TOOLBOX_DIR}/communications)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/toolbox/communications/src OUTPUT_FILE libcommunications.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${COMMUNICATIONS_SRCS})" )  
INSTALL( FILES libcommunications.def DESTINATION ${FD_TOOLBOX_DIR}/communications/)

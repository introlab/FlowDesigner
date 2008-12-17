	
SET(COMMUNICATIONS_SRCS
	iidSource.cc
	DiscreteSource.cc
	ComplexGaussian.cc
	Constellation.cc
	LLRComputer.cc
	MLDetector.cc
	BitErrors.cc
	MutualInf.cc
	Histogram.cc
)	

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	../include 
	${LIBXML2_INCLUDE} )
	
add_library(libcommunications-dynamic SHARED ${COMMUNICATIONS_SRCS})
	
set_target_properties(libcommunications-dynamic PROPERTIES OUTPUT_NAME communications.tlb)

target_link_libraries(libcommunications-dynamic libflow-dynamic ${LIBXML2_LIBRARIES})

install(TARGETS libcommunications-dynamic DESTINATION ${FD_TOOLBOX_DIR}/communications)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/toolbox/communications/src OUTPUT_FILE libcommunications.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${COMMUNICATIONS_SRCS})" )  
INSTALL( FILES libcommunications.def DESTINATION ${FD_TOOLBOX_DIR}/communications/)

	
SET(VQ_SRCS
	kmeans.cc
	Cell.cc
	MMITrain.cc
	VQuantize.cc
	VQuantizeDiff.cc
	MMIScore.cc
	VQTrain.cc
	VQClass.cc
	CMCalc.cc
	CMTrain.cc
	CodebookMap.cc
	VQCloseness.cc
	VQWeightMeans.cc
	msvq.cc
	MSVQTrain.cc
	RBF.cc
	RBFTrain.cc
	FMapCalc.cc
	FMapTrain.cc
	FeatureMap.cc
)	

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	../include 
	${LIBXML2_INCLUDE} )
	
add_library(libvq-dynamic SHARED ${VQ_SRCS})
	
set_target_properties(libvq-dynamic PROPERTIES OUTPUT_NAME libvq)

target_link_libraries(libvq-dynamic -L${FLOWDESIGNER_SOURCE_DIR}/data-flow/src -llibflow ${LIBXML2_LIBRARIES})

install(TARGETS libvq-dynamic DESTINATION toolbox/bin)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/VQ/src OUTPUT_FILE ${CMAKE_INSTALL_PREFIX}/libvq.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${VQ_SRCS})" )  


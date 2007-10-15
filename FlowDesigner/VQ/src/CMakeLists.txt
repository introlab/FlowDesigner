	
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
	
set_target_properties(libvq-dynamic PROPERTIES OUTPUT_NAME libvq.tlb)

target_link_libraries(libvq-dynamic libflow-dynamic ${LIBXML2_LIBRARIES})

install(TARGETS libvq-dynamic DESTINATION ${FD_TOOLBOX_DIR}/VQ)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/VQ/src OUTPUT_FILE libvq.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${VQ_SRCS})" )  
INSTALL( FILES libvq.def DESTINATION ${FD_TOOLBOX_DIR}/VQ/)

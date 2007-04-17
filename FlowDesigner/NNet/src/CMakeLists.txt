	
SET(NNET_SRCS
	FFLayer.cc 
	FFNet.cc 
	NNetExec.cc 
	NNetTrainDBD.cc
	NNetSet.cc 
	NNetSetCalc.cc
	NNetSetTrainDBD.cc
	NNetInit.cc
	NNetSetInit.cc
	NNetSetChooseBest.cc
	NNetTrainQProp.cc
	NNetTrainSCG.cc
	TrainingAlgo.cc
	NNetTrainWeightDBD.cc
)	

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	../include 
	${LIBXML2_INCLUDE} )
	
add_library(libnnet-dynamic SHARED ${NNET_SRCS})
	
set_target_properties(libnnet-dynamic PROPERTIES OUTPUT_NAME libnnet.tlb)

target_link_libraries(libnnet-dynamic -L${FLOWDESIGNER_SOURCE_DIR}/data-flow/src -llibflow ${LIBXML2_LIBRARIES})

install(TARGETS libnnet-dynamic DESTINATION ${FD_TOOLBOX_DIR}/NNET)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/NNet/src OUTPUT_FILE ${CMAKE_INSTALL_PREFIX}/${FD_TOOLBOX_DIR}/NNET/libnnet.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${NNET_SRCS})" )  


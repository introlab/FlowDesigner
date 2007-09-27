	
SET(HMM_SRCS
	gmm.cc 
	covariance.cc 
	gaussian.cc
	audioinfo.cc 
	acoustic_model.cc 
	covariance_set.cc
	mean_set.cc 
	gaussian_set.cc 
	gmm_set.cc 
	mean.cc
	GMMScore.cc 
	GMMTrain.cc 
	TransMatrix.cc 
	MarkovProb.cc 
	AdaptMAP.cc
	DiagGMM.cc 
	MakeDiagGMM.cc 
	DiagGMMScore.cc 
	DiagGMMSetScore.cc 
	DiagGMMTrain.cc
)	

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	../include 
	${LIBXML2_INCLUDE} )
	
add_library(libhmm-dynamic SHARED ${HMM_SRCS})
	
set_target_properties(libhmm-dynamic PROPERTIES OUTPUT_NAME libhmm.tlb)

target_link_libraries(libhmm-dynamic -L${FLOWDESIGNER_SOURCE_DIR}/data-flow/src -llibflow ${LIBXML2_LIBRARIES})

install(TARGETS libhmm-dynamic DESTINATION ${FD_TOOLBOX_DIR}/HMM)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/HMM/src OUTPUT_FILE libhmm.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${HMM_SRCS})" )  
INSTALL( FILES libhmm.def DESTINATION ${FD_TOOLBOX_DIR}/HMM/)

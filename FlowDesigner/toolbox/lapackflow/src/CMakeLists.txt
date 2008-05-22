SET(LAPACKFLOW_SRCS
	Eig.cc	
	lapackflow.cc 
	LinSolve.cc 
	SolveLS.cc
	SVD.cc
	flapack.f
)	

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	../include 
	${LIBXML2_INCLUDE} )
	
add_library(liblapackflow-dynamic SHARED ${LAPACKFLOW_SRCS})
	
set_target_properties(liblapackflow-dynamic PROPERTIES OUTPUT_NAME lapackflow.tlb)

target_link_libraries(liblapackflow-dynamic libflow-dynamic ${LIBXML2_LIBRARIES} ${LAPACK_LIBRARIES})

install(TARGETS liblapackflow-dynamic DESTINATION ${FD_TOOLBOX_DIR}/LAPACKFLOW)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/toolbox/lapackflow/src OUTPUT_FILE liblapackflow.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${LAPACKFLOW_SRCS})" )  
INSTALL( FILES liblapackflow.def DESTINATION ${FD_TOOLBOX_DIR}/LAPACKFLOW/)

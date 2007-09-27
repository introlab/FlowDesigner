#finding qt
find_package(Qt4 REQUIRED)
include(${QT_USE_FILE})	

SET(FDQTTCP_EXAMPLES
 exemple/listenJitterMatrix.n
 exemple/listenString.n
 exemple/sendString.n
 exemple/sendWebcam.n
 )
 
SET(FDQTTCP_SRCS
   QtSendJitterMatrix.cc
   QtListenJitterMatrix.cc
   QtSendString.cc
   QtListenString.cc
)

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
     	./
	${LIBXML2_INCLUDE} 
	${QT_INCLUDE_DIR}
)
add_library(libqttcp-dynamic SHARED ${FDQTTCP_SRCS})
	
set_target_properties(libqttcp-dynamic PROPERTIES OUTPUT_NAME libqttcp.tlb)

target_link_libraries(libqttcp-dynamic -LC${QT_LIBRARIES} -L${FLOWDESIGNER_SOURCE_DIR}/data-flow/src -llibflow ${LIBXML2_LIBRARIES}
                -LC:/Qt/4.3.0/bin -lQtNetwork4
)

install(TARGETS libqttcp-dynamic DESTINATION ${FD_TOOLBOX_DIR}/FDQTTCP)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/FDQtTCP OUTPUT_FILE libqttcp.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${FDQTTCP_SRCS})" )  
INSTALL( FILES libqttcp.def DESTINATION ${FD_TOOLBOX_DIR}/FDQTTCP/)
INSTALL( FILES ${FDQTTCP_EXAMPLES} DESTINATION examples/FDQTTCP/)


# the next line sets up include and link directories and defines some variables that we will use.
# you can modify the behavior by setting some variables, e.g.
#   set(QT_USE_OPENGL TRUE)
# -> this will cause cmake to include and link against the OpenGL module
SET(QT_USE_QTNETWORK TRUE)

include(${QT_USE_FILE})

SET(QTFLOW_SRCS 
	QtLink.cc  
	QtNetwork.cc  
	QtNode.cc 
	QtTerminal.cc
	QtFlowDesigner.cc
	QtNodeTreeView.cc
	QtDocument.cc
	QtNetTerminal.cc
	QtDLManager.cc
	QtRunContext.cc
	QtNodeParameters.cc
	QtProcessWindow.cc
	UIDocumentController.cc
	UINetworkController.cc
	UINodeController.cc
	UITerminalController.cc
	UINetTerminalController.cc
	UILinkController.cc
)
SET(FDQTTCP_SRCS
    QtSendJitterMatrix.cc
    QtListenJitterMatrix.cc
    QtSendString.cc
    QtListenString.cc	
)

SET(QTFLOW_HEADERS 
	../include/QtLink.h
	../include/QtNetwork.h
	../include/QtNode.h
	../include/QtTerminal.h
	../include/QtFlowDesigner.h
	../include/QtNodeTreeView.h
	../include/QtDocument.h
	../include/QtNetTerminal.h
	../include/QtDLManager.h
	../include/QtRunContext.h
	../include/QtNodeParameters.h
	../include/QtProcessWindow.h
	../include/UIDocumentController.h
	../include/UINetworkController.h
	../include/UINodeController.h
	../include/UITerminalController.h
	../include/UINetTerminalController.h
	../include/UILinkController.h
)

#This will generate moc_* for Qt
QT4_WRAP_CPP(QTFLOW_MOC_SRCS ${QTFLOW_HEADERS})

#include files
INCLUDE_DIRECTORIES(../include ${FLOWDESIGNER_SOURCE_DIR}/data-flow/include ${QT_INCLUDE_DIR} ${LIBXML2_INCLUDE_DIR})

# create an executable file named "flowdesigner" from the source files 
add_executable(flowdesigner ${QTFLOW_SRCS} main.cc ${QTFLOW_MOC_SRCS})

# create an executable file named "qtflow" from the source files 
add_executable(qtflow ${QTFLOW_SRCS} qtflow.cc ${QTFLOW_MOC_SRCS})

# Linking with libflow and Qt libraries
target_link_libraries(flowdesigner  ${QT_LIBRARIES} libflow-dynamic ${LIBXML2_LIBRARIES})
target_link_libraries(qtflow  ${QT_LIBRARIES} libflow-dynamic ${LIBXML2_LIBRARIES})



add_library(libfdqttcp-dynamic SHARED ${FDQTTCP_SRCS})
	
set_target_properties(libfdqttcp-dynamic PROPERTIES OUTPUT_NAME libfdqttcp.tlb)

target_link_libraries(libfdqttcp-dynamic libflow-dynamic ${QT_LIBRARIES} ${LIBXML2_LIBRARIES})

install(TARGETS libfdqttcp-dynamic DESTINATION ${FD_TOOLBOX_DIR}/FDQTTCP)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/qtflow/src OUTPUT_FILE libfdqttcp.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${FDQTTCP_SRCS})" )  
INSTALL( FILES ${FLOWDESIGNER_SOURCE_DIR}/qtflow/src/libfdqttcp.def DESTINATION ${FD_TOOLBOX_DIR}/FDQTTCP/)










install(TARGETS flowdesigner DESTINATION bin)
install(TARGETS qtflow DESTINATION bin)

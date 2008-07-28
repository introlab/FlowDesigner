
# the next line sets up include and link directories and defines some variables that we will use.
# you can modify the behavior by setting some variables, e.g.
#   set(QT_USE_OPENGL TRUE)
# -> this will cause cmake to include and link against the OpenGL module
SET(QT_USE_QTNETWORK TRUE)
SET(QT_USE_QTSVG TRUE)
SET(QT_USE_QTXML TRUE)
SET(QT_USE_QTWEBKIT TRUE)

include(${QT_USE_FILE})

SET(QTFLOW_SRCS 
	QtLink.cc  
	QtNetwork.cc
    	QtNetworkScene.cc  
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
	QtProbeManager.cc
    QtProbeConsole.cc
	QtNote.cc
	QtDocumentParameters.cc
	QtFlowDesignerSplash.cc
    ./iconeditor/QtIconEditor.cc
    ./iconeditor/QtIconItem.cc
    ./iconeditor/QtIconScene.cc
    ./iconeditor/QtRectItem.cc
    ./iconeditor/QtLineItem.cc
    ./iconeditor/QtEllipseItem.cc
    ./iconeditor/QtPolygonItem.cc
    ./iconeditor/QtVertex.cc
)

SET(QTFLOW_HEADERS 
	../include/QtLink.h
	../include/QtNetwork.h
    	../include/QtNetworkScene.h
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
	../include/QtNote.h
	../include/QtDocumentParameters.h
	../include/QtFlowDesignerSplash.h
	../include/QtProbeManager.h
    ../include/QtProbeConsole.h
	../include/QtIORedirector.h
    	../include/QtConnectDialog.h
    	../include/iconeditor/QtIconEditor.h
    	../include/iconeditor/QtIconItem.h
    	../include/iconeditor/QtIconScene.h
    	../include/iconeditor/QtRectItem.h
    	../include/iconeditor/QtLineItem.h
    	../include/iconeditor/QtEllipseItem.h
    	../include/iconeditor/QtPolygonItem.h
    	../include/iconeditor/QtVertex.h
)

SET(FDQTTCP_SRCS
    QtSendJitterMatrix.cc
    QtListenJitterMatrix.cc
    QtSendString.cc
    QtListenString.cc
    TextProbe.cc	
)

SET(QTFLOWLIB_SRCS
    QtProbe.cc
)

SET(QTFLOWLIB_HEADERS
	../include/QtProbeRegistry.h
    ../include/QtProbe.h
)

SET(QTFLOW_RCCS ../flowdesigner.qrc)

# generate rules for building source files from the resources
QT4_ADD_RESOURCES(QTFLOW_RCC_SRCS ${QTFLOW_RCCS})

#This will generate moc_* for Qt
QT4_WRAP_CPP(QTFLOW_MOC_SRCS ${QTFLOW_HEADERS})

#This will generate moc_* for Qt
QT4_WRAP_CPP(QTFLOWLIB_MOC_SRCS ${QTFLOWLIB_HEADERS})

#include files
INCLUDE_DIRECTORIES(../include ${FLOWDESIGNER_SOURCE_DIR}/data-flow/include ${QT_INCLUDE_DIR} ${LIBXML2_INCLUDE_DIR})

# Add QTFLOW library, used for Qt Probes
add_library(libqtflow-dynamic SHARED ${QTFLOWLIB_SRCS} ${QTFLOWLIB_MOC_SRCS})
set_target_properties(libqtflow-dynamic PROPERTIES VERSION ${CPACK_PACKAGE_VERSION} SOVERSION ${CPACK_PACKAGE_VERSION} OUTPUT_NAME qtflow)
target_link_libraries(libqtflow-dynamic ${QT_LIBRARIES})
install(TARGETS libqtflow-dynamic DESTINATION lib
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib 
)
#Install headers
INSTALL(FILES ${QTFLOWLIB_HEADERS} DESTINATION include/flowdesigner/qtflow)

# create an executable file named "flowdesigner" from the source files
IF(APPLE)
     SET(MACOSX_BUNDLE_INFO_STRING "FlowDesigner App")
     SET(MACOSX_BUNDLE_ICON_FILE "FlowDesignerIcon.icns")
     SET(MACOSX_BUNDLE_GUI_IDENTIFIER "flowdesigner")
     SET(MACOSX_BUNDLE_LONG_VERSION_STRING "flowdesigner ${VERSION}")
     SET(MACOSX_BUNDLE_BUNDLE_NAME "flowdesigner")
     SET(MACOSX_BUNDLE_SHORT_VERSION_STRING "${VERSION}")
     SET(MACOSX_BUNDLE_BUNDLE_VERSION "${VERSION}")
     SET(MACOSX_BUNDLE_COPYRIGHT "Dominic Letourneau, Jean-Marc Valin") 
     add_executable(flowdesigner MACOSX_BUNDLE ${QTFLOW_SRCS} main.cc ${QTFLOW_MOC_SRCS} ${QTFLOW_RCC_SRCS})
    #install( FILES ../icons/FlowDesignerIcon.icns DESTINATION /Applications/flowdesigner.app/Contents/Resources)
ELSE(APPLE)
    add_executable(flowdesigner ${QTFLOW_SRCS} main.cc ${QTFLOW_MOC_SRCS} ${QTFLOW_RCC_SRCS})
ENDIF(APPLE)

# create an executable file named "qtflow" from the source files 
add_executable(qtflow ${QTFLOW_SRCS} qtflow.cc ${QTFLOW_MOC_SRCS})

# Linking with libflow and Qt libraries
target_link_libraries(flowdesigner  ${QT_LIBRARIES} libflow-dynamic ${LIBXML2_LIBRARIES} libqtflow-dynamic)
target_link_libraries(qtflow  ${QT_LIBRARIES} libflow-dynamic ${LIBXML2_LIBRARIES} libqtflow-dynamic)

# Add FDQTTCP library
add_library(libfdqttcp-dynamic SHARED ${FDQTTCP_SRCS})
set_target_properties(libfdqttcp-dynamic PROPERTIES OUTPUT_NAME fdqttcp.tlb)
target_link_libraries(libfdqttcp-dynamic libflow-dynamic ${QT_LIBRARIES} ${LIBXML2_LIBRARIES})
install(TARGETS libfdqttcp-dynamic DESTINATION ${FD_TOOLBOX_DIR}/FDQTTCP)

#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/qtflow/src OUTPUT_FILE libfdqttcp.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${FDQTTCP_SRCS})" )  
INSTALL( FILES ${FLOWDESIGNER_SOURCE_DIR}/qtflow/src/libfdqttcp.def DESTINATION ${FD_TOOLBOX_DIR}/FDQTTCP/)

INSTALL( FILES "FlowDesignerSplashScreen.jpg" DESTINATION "share/flowdesigner")


IF(APPLE)
    install(TARGETS flowdesigner DESTINATION "/Applications")
ELSE(APPLE)
    install(TARGETS flowdesigner DESTINATION bin)
ENDIF(APPLE)

install(TARGETS qtflow DESTINATION bin)

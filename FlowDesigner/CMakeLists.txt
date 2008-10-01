PROJECT (FlowDesigner)

#Look for minimum cmake version
cmake_minimum_required(VERSION 2.6.0)

# This line will tell CMake to look in the project directory for configure scripts
SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/macros)
SET(FLOWDESIGNER_SOURCE_DIR ${PROJECT_SOURCE_DIR})

# compile in debug mode
IF(NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE Release)
ENDIF(NOT CMAKE_BUILD_TYPE)

IF(WIN32)
	SET(CMAKE_INSTALL_PREFIX "C:/Progra~1/FlowDesigner")
ELSE(WIN32)
	IF(APPLE)
		SET(CMAKE_INSTALL_PREFIX "/usr/local")
	ELSE(APPLE)
		SET(CMAKE_INSTALL_PREFIX "/usr/local")
	ENDIF(APPLE)
ENDIF(WIN32)

SET(VERSION 0.10.0)
SET(CPACK_PACKAGE_VERSION "${VERSION}")
SET(CPACK_PACKAGE_VERSION_MAJOR "0")
SET(CPACK_PACKAGE_VERSION_MINOR "10")
SET(CPACK_PACKAGE_VERSION_PATCH "0")

SET(PEDANTIC TRUE)


# skip the full RPATH for the build tree
#SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, use the install RPATH already
# (so it doesn't need to relink when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE) 

# the RPATH to be used when installing
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
#SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)



SET(CMAKE_VERBOSE_MAKEFILE ON)
SET(FD_TOOLBOX_DIR lib/flowdesigner/toolbox)
SET(FD_ICONS_DIR share/flowdesigner/icons)

ADD_DEFINITIONS(-DFLOWDESIGNER_VERSION=\"${VERSION}\")
ADD_DEFINITIONS(-DINSTALL_PREFIX=\"${CMAKE_INSTALL_PREFIX}\")
ADD_DEFINITIONS(-DFD_ICONS_PATH=\"${CMAKE_INSTALL_PREFIX}/${FD_ICONS_DIR}\")

#Testing multithreading
IF(NOT WIN32)
	ADD_DEFINITIONS(-DSMP)
	ADD_DEFINITIONS(-D_ENABLE_X86_ASM)
	ADD_DEFINITIONS(-DMULTITHREAD)
ENDIF(NOT WIN32)

ADD_DEFINITIONS(-DHAVE_FLOAT_H)

IF("${CMAKE_SYSTEM}" MATCHES "Linux")
	ADD_DEFINITIONS(-DHAVE_VALUES_H)
ENDIF("${CMAKE_SYSTEM}" MATCHES "Linux")

#THIS WILL CALL XML2 MODULE IN macros DIRECTORY
find_package(LibXml2 REQUIRED)

#STANDARD PERL MODULE
find_package(Perl REQUIRED)

#GTEST MODULE
find_package(GTest)


IF(UNIX AND NOT WIN32)
	ADD_DEFINITIONS(-DHAVE_DLFCN_H)
ENDIF(UNIX AND NOT WIN32)

ADD_SUBDIRECTORY(data-flow)
ADD_SUBDIRECTORY(qtflow)
ADD_SUBDIRECTORY(toolbox)
ADD_SUBDIRECTORY(tools)
ADD_SUBDIRECTORY(examples)
SET(INSTALLED_EXAMPLES
 examples/listenJitterMatrix.n
 examples/listenString.n
 examples/sendString.n
 examples/sendWebcam.n
 examples/sendWavOverTcp.n
)

SET(INSTALLED_TUTORIALS
 examples/tutorials/HelloWorld.n
 examples/tutorials/HelloWorldLoop.n
 examples/tutorials/Probing.n
)

SET( ENV{FLOWDESIGNER_HOME} ${INSTALL_PREFIX} )

INCLUDE(InstallRequiredSystemLibraries)

IF (WIN32)
	#copy libxml
	SET(QTBIN_DIR ${QT_LIBRARY_DIR}/../bin)
	
	SET(DEVCPP_DLL
	C:/Dev-Cpp/bin/libxml2.dll
 	C:/Dev-Cpp/bin/iconv.dll
 	C:/MinGW/bin/mingwm10.dll
 	C:/Dev-Cpp/bin/zlib1.dll
	${QTBIN_DIR}/Qt3Support4.dll
	${QTBIN_DIR}/QtAssistantClient4.dll
	${QTBIN_DIR}/QtCore4.dll
	${QTBIN_DIR}/QtDesigner4.dll
	${QTBIN_DIR}/QtDesignerComponents4.dll
	${QTBIN_DIR}/QtGui4.dll
	${QTBIN_DIR}/QtNetwork4.dll
	${QTBIN_DIR}/QtOpenGL4.dll
	${QTBIN_DIR}/QtScript4.dll
	${QTBIN_DIR}/QtSql4.dll
	${QTBIN_DIR}/QtSvg4.dll
	${QTBIN_DIR}/QtTest4.dll
	${QTBIN_DIR}/QtXml4.dll
	${QTBIN_DIR}/QtWebKit4.dll
	)
	INSTALL(FILES ${DEVCPP_DLL} DESTINATION bin)
ENDIF(WIN32)




IF(APPLE)
	SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/license.txt")
ELSE(APPLE)
	SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING.LGPL")
ENDIF(APPLE)


    SET(CPACK_PACKAGE_NAME "FlowDesigner")
    SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "FlowDesigner")
    SET(CPACK_PACKAGE_VENDOR "Dominic Letourneau, Jean-Marc Valin")
    SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
    SET(CPACK_PACKAGE_CONTACT "flowdesigner-devel@lists.sourceforge.net")

IF(WIN32 AND NOT UNIX)
  SET(CPACK_PACKAGE_INSTALL_DIRECTORY "FlowDesigner")
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  # SET(CPACK_PACKAGE_ICON "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
  # SET(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\FDSetup.exe")
  # SET(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} FlowDesigner")
  # SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\flowdesigner.sourceforge.net")
  SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\flowdesigner.sourceforge.net")
  SET(CPACK_NSIS_CONTACT "flowdesigner-devel@lists.sourceforge.net")
  SET(CPACK_NSIS_MODIFY_PATH ON)
ENDIF(WIN32 AND NOT UNIX)

SET(CPACK_PACKAGE_EXECUTABLES "flowdesigner" "FlowDesigner" "batchflow" "batchflow" "qtflow" "qtflow")

INSTALL( FILES ${INSTALLED_EXAMPLES} DESTINATION share/flowdesigner/examples)
INSTALL( FILES ${INSTALLED_TUTORIALS} DESTINATION share/flowdesigner/examples/tutorials)

MESSAGE(STATUS "Writing pkg-config file...")
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/FlowDesigner.pc.cmake ${CMAKE_BINARY_DIR}/FlowDesigner.pc @ONLY)
INSTALL( FILES ${CMAKE_BINARY_DIR}/FlowDesigner.pc DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/pkgconfig )

MESSAGE(STATUS "Writing flowdesigner-config file...")
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/flowdesigner-config.cmake ${CMAKE_BINARY_DIR}/flowdesigner-config @ONLY)
INSTALL( FILES ${CMAKE_BINARY_DIR}/flowdesigner-config DESTINATION ${CMAKE_INSTALL_PREFIX}/bin PERMISSIONS OWNER_READ GROUP_READ WORLD_READ WORLD_EXECUTE)

SET(CPACK_DEBIAN_PACKAGE_NAME "flowdesigner")
SET(CPACK_DEBIAN_PACKAGE_VERSION "snapshot-23_05_2008")
SET(CPACK_DEBIAN_ARCHITECTURE i386)
SET(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>= 2.6-1), libfontconfig1 (>= 2.4.0), libfreetype6 (>= 2.3.5), libgcc1 (>= 1:4.2.1), libice6 (>= 1:1.0.0), libpng12-0 (>= 1.2.13-4), libqt4-core (>= 4.3.2), libqt4-gui (>= 4.3.2), libqt4-network (>= 4.3.2), libqt4-svg (>= 4.3.2), libsm6, libstdc++6 (>= 4.2.1), libx11-6, libxcursor1 (>> 1.1.2), libxext6, libxi6, libxinerama1, libxml2 (>= 2.6.29), libxrandr2 (>= 2:1.2.0), libxrender1, zlib1g (>= 1:1.2.3.3.dfsg-1)")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "flowdesigner-devel@lists.sourceforge.net")
SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION "FlowDesigner description not available right now.")
SET(CPACK_DEBIAN_PACKAGE_SECTION "devel")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
SET(CPACK_DEBIAN_PACKAGE_RECOMMENDS "")
SET(CPACK_DEBIAN_PACKAGE_SUGGESTS "")

INCLUDE(CPack)
INCLUDE(UseDebian)
 IF(DEBIAN_FOUND)
  ADD_DEBIAN_TARGETS(GDCM)
ENDIF(DEBIAN_FOUND)


PROJECT (FlowDesigner)

#Look for minimum cmake version
cmake_minimum_required(VERSION 2.4.0)

# This line will tell CMake to look in the project directory for configure scripts
SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/macros)
SET(FLOWDESIGNER_SOURCE_DIR ${PROJECT_SOURCE_DIR})

SET(CMAKE_BUILD_TYPE Release )





IF(WIN32)
	SET(CMAKE_INSTALL_PREFIX "C:/Progra~1/FlowDesigner")
ELSE(WIN32)
	IF(APPLE)
		#SET(CMAKE_INSTALL_PREFIX "/Applications/FlowDesigner.app")
	ELSE(APPLE)
		SET(CMAKE_INSTALL_PREFIX "/usr")
	ENDIF(APPLE)
ENDIF(WIN32)

SET(PEDANTIC TRUE)


# skip the full RPATH for the build tree
#SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, use the install RPATH already
# (so it doesn't need to relink when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE) 

# the RPATH to be used when installing
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
#SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)



SET(CMAKE_VERBOSE_MAKEFILE ON)
SET(FD_TOOLBOX_DIR lib/flowdesigner/toolbox)
SET(FD_ICONS_DIR share/flowdesigner/icons)

ADD_DEFINITIONS(-DFLOWDESIGNER_VERSION=\\\"0.9.1\\\")
ADD_DEFINITIONS(-DINSTALL_PREFIX=\\\"${CMAKE_INSTALL_PREFIX}\\\")
ADD_DEFINITIONS(-DFD_ICONS_PATH=\\\"${CMAKE_INSTALL_PREFIX}/${FD_ICONS_DIR}\\\")
ADD_DEFINITIONS(-DHAVE_FLOAT_H)

IF("${CMAKE_SYSTEM}" MATCHES "Linux")
	ADD_DEFINITIONS(-DHAVE_VALUES_H)
ENDIF("${CMAKE_SYSTEM}" MATCHES "Linux")

#THIS WILL CALL XML2 MODULE IN macros DIRECTORY
find_package(LibXml2 REQUIRED)

#STANDARD PERL MODULE
find_package(Perl REQUIRED)

IF(UNIX AND NOT WIN32)
	ADD_DEFINITIONS(-DHAVE_DLFCN_H)
ENDIF(UNIX AND NOT WIN32)


ADD_SUBDIRECTORY(data-flow)
ADD_SUBDIRECTORY(qtflow)
ADD_SUBDIRECTORY(FuzzyEngine)
ADD_SUBDIRECTORY(NNet)
ADD_SUBDIRECTORY(VQ)
ADD_SUBDIRECTORY(HMM)
ADD_SUBDIRECTORY(audio_blocks)
ADD_SUBDIRECTORY(effects)
ADD_SUBDIRECTORY(tools)
ADD_SUBDIRECTORY(examples)

SET(INSTALLED_EXAMPLES
 examples/listenJitterMatrix.n
 examples/listenString.n
 examples/sendString.n
 examples/sendWebcam.n
)

SET( ENV{FLOWDESIGNER_HOME} ${INSTALL_PREFIX} )

INCLUDE(InstallRequiredSystemLibraries)

IF (WIN32)
	#copy libxml
	SET(DEVCPP_DLL
 	C:/Dev-Cpp/bin/libxml2.dll
 	C:/Dev-Cpp/bin/iconv.dll
 	C:/Dev-Cpp/bin/charset.dll
 	C:/Dev-Cpp/bin/mingwm10.dll
 	C:/Dev-Cpp/bin/zlib1.dll
	)
	INSTALL(FILES ${DEVCPP_DLL} DESTINATION bin)
ENDIF(WIN32)

SET(CPACK_PACKAGE_NAME "FlowDesigner")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "FlowDesigner")
SET(CPACK_PACKAGE_VENDOR "Dominic Létourneau, Jean-Marc Valin")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")

IF(APPLE)
	SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/license.txt")
ELSE(APPLE)
	SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING.LGPL")
ENDIF(APPLE)

SET(CPACK_PACKAGE_VERSION_MAJOR "0")
SET(CPACK_PACKAGE_VERSION_MINOR "9")
SET(CPACK_PACKAGE_VERSION_PATCH "1")
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "FlowDesigner")
IF(WIN32 AND NOT UNIX)
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

INCLUDE(CPack)

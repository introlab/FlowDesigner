PROJECT (FlowDesigner)

#Look for minimum cmake version
cmake_minimum_required(VERSION 2.4.0)

# This line will tell CMake to look in the project directory for configure scripts
SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/macros)

SET( CMAKE_BUILD_TYPE Release )
SET(CMAKE_INSTALL_PREFIX "C:/FlowDesigner")
SET(CMAKE_VERBOSE_MAKEFILE ON)
SET(FD_TOOLBOX_DIR lib/flowdesigner/toolbox)


ADD_DEFINITIONS(-DFLOWDESIGNER_VERSION=\\\"TESTING\\\")
ADD_DEFINITIONS(-DINSTALL_PREFIX=\\\"C:/FlowDesigner\\\")
ADD_DEFINITIONS(-DHAVE_FLOAT_H)
ADD_DEFINITIONS(-DHAVE_VALUES_H)

find_package(libxml2 REQUIRED)
find_package(perl REQUIRED)
#find_package(OpenCV REQUIRED)


SUBDIRS(${FLOWDESIGNER_SOURCE_DIR}/data-flow 
	${FLOWDESIGNER_SOURCE_DIR}/qtflow 
	${FLOWDESIGNER_SOURCE_DIR}/FuzzyEngine
	${FLOWDESIGNER_SOURCE_DIR}/NNet
	${FLOWDESIGNER_SOURCE_DIR}/VQ
	${FLOWDESIGNER_SOURCE_DIR}/HMM
	${FLOWDESIGNER_SOURCE_DIR}/audio_blocks
	${FLOWDESIGNER_SOURCE_DIR}/effects
	${FLOWDESIGNER_SOURCE_DIR}/tools
	)

INCLUDE(InstallRequiredSystemLibraries)

SET(CPACK_PACKAGE_NAME "FlowDesigner")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "FlowDesigner")
SET(CPACK_PACKAGE_VENDOR "Dominic Letourneau, Jean-Marc Valin")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING.LGPL")
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

SET(CPACK_PACKAGE_EXECUTABLES "batchflow;flowdesigner")

INCLUDE(CPack)
PROJECT (FlowDesigner)

#Look for minimum cmake version
cmake_minimum_required(VERSION 2.6.0)


set(CMAKE_CXX_STANDARD 11)

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
		SET(CMAKE_INSTALL_PREFIX "/opt/flowdesigner/")
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

#enable all warnings
ADD_DEFINITIONS ( -Wall )

SET (EXECUTABLE_OUTPUT_PATH ${FLOWDESIGNER_SOURCE_DIR}/bin)


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
#	ADD_DEFINITIONS(-DSMP)
#	ADD_DEFINITIONS(-D_ENABLE_X86_ASM)
#	ADD_DEFINITIONS(-DMULTITHREAD)
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

ADD_SUBDIRECTORY(data-flow)
#ADD_SUBDIRECTORY(qtflow)
#ADD_SUBDIRECTORY(toolbox)
#ADD_SUBDIRECTORY(tools)
#add_subdirectory(package)


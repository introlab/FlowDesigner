# - Find GTEST library
# Find the native GTEST includes and library
# This module defines
#  GTEST_INCLUDE_DIR, where to find gtest.h, etc.
#  GTEST_LIBRARIES, libraries to link against to use GTEST.
#  GTEST_FOUND, If false, do not try to use GTEST.
# also defined, but not for general use are
#  GTEST_LIBRARY, where to find the GTEST library.

FIND_PATH(GTEST_INCLUDE_DIR gtest.h 
    /usr/include/gtest
    /opt/local/include/gtest
    /usr/local/include/gtest
)

FIND_LIBRARY(GTEST_LIBRARY NAMES "gtest" PATH /usr/lib /usr/local/lib /opt/local/lib)
FIND_LIBRARY(GTEST_MAIN_LIBRARY NAMES "gtest_main" PATH /usr/lib /usr/local/lib /opt/local/lib)

# handle the QUIETLY and REQUIRED arguments and set GTEST_FOUND to TRUE if 
# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(GTEST DEFAULT_MSG GTEST_LIBRARY GTEST_INCLUDE_DIR)

IF(GTEST_FOUND)
  SET( GTEST_LIBRARIES ${GTEST_LIBRARY} ${GTEST_MAIN_LIBRARY} )
  SET( GTEST_INCLUDES "${GTEST_INCLUDE_DIR}/../")
  MESSAGE(STATUS "Found GTEST libraries: ${GTEST_LIBRARIES}")
  MESSAGE(STATUS "Found GTEST includes : ${GTEST_INCLUDE_DIR}")
ENDIF(GTEST_FOUND)

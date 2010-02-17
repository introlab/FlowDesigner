
SET(QT_USE_QTNETWORK TRUE)
SET(QT_USE_QTSVG TRUE)
SET(QT_USE_QTXML TRUE)
SET(QT_USE_QTWEBKIT TRUE)

find_package(Qt4 REQUIRED)

include(${QT_USE_FILE})
include_directories(${QT_INCLUDE_DIR})
include_directories(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include)


SET(STRING_SRCS
	LowerChar.cc
	UpperChar.cc
	StringSplit.cc
)

#INSTALLING .def file 
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/toolbox/string/src OUTPUT_FILE libstring.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${STRING_SRCS})" )  
INSTALL( FILES libstring.def DESTINATION ${FD_TOOLBOX_DIR}/STRING/)


# make a library
add_library(libstring-dynamic SHARED ${STRING_SRCS})

# rename libstring-dynamic in string.tlb
set_target_properties(libstring-dynamic PROPERTIES OUTPUT_NAME string.tlb)

# install the library in the STRING directory
install(TARGETS libstring-dynamic DESTINATION ${FD_TOOLBOX_DIR}/STRING)

# link the library
target_link_libraries(libstring-dynamic libflow-dynamic ${QT_LIBRARIES} ${LIBXML2_LIBRARIES})

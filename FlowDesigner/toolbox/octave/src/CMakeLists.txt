SET(OCTAVE_SRCS
	OctaveFullScriptPath.cc 
	OctaveParser.cc
)	

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	../include 
	${LIBXML2_INCLUDE}
	${OCTAVE_INCLUDES} )
	
add_library(liboctave-dynamic SHARED ${OCTAVE_SRCS})

ADD_DEFINITIONS( -DOCTAVE_SCRIPTS_PATH="${CMAKE_INSTALL_PREFIX}/lib/flowdesigner/toolbox/Octave/scripts" )

set_target_properties(liboctave-dynamic PROPERTIES OUTPUT_NAME octave.tlb)

target_link_libraries(liboctave-dynamic libflow-dynamic ${LIBXML2_LIBRARIES} ${OCTAVE_LIBRARIES})

install(TARGETS liboctave-dynamic DESTINATION ${FD_TOOLBOX_DIR}/OCTAVE)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/toolbox/octave/src OUTPUT_FILE liboctave.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${OCTAVE_SRCS})" )  
INSTALL( FILES liboctave.def DESTINATION ${FD_TOOLBOX_DIR}/OCTAVE/)

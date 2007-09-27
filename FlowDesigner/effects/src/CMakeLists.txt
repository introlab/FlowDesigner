	
SET(EFFECTS_SRCS
	Saturate.cc 
	rev.cc 
	Reverb.cc
)	

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	../include 
	${LIBXML2_INCLUDE} )
	
add_library(libeffects-dynamic SHARED ${EFFECTS_SRCS})
	
set_target_properties(libeffects-dynamic PROPERTIES OUTPUT_NAME libeffects.tlb)

target_link_libraries(libeffects-dynamic -L${FLOWDESIGNER_SOURCE_DIR}/data-flow/src -llibflow ${LIBXML2_LIBRARIES})

install(TARGETS libeffects-dynamic DESTINATION ${FD_TOOLBOX_DIR}/EFFECTS)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/effects/src OUTPUT_FILE libeffect.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${EFFECTS_SRCS})" )  
INSTALL( FILES ${FLOWDESIGNER_SOURCE_DIR}/effects/src/libeffect.def DESTINATION ${FD_TOOLBOX_DIR}/EFFECTS/)

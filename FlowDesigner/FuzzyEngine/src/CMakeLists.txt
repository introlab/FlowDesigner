	
SET(FUZZY_SRCS
	FuzzyFunction.cc
	FuzzyModel.cc
	FuzzyRule.cc
	FuzzySet.cc
	GenericModel.cc
	TrapezoidalFunction.cc
	TriangularFunction.cc
	FuzzyOperators.cc
	FuzzyModelExec.cc
	Tokenizer.cc
	InferenceModel.cc
)	

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
	../include 
	${LIBXML2_INCLUDE} )
	
add_library(libfuzzy-dynamic SHARED ${FUZZY_SRCS})
	
set_target_properties(libfuzzy-dynamic PROPERTIES OUTPUT_NAME libfuzzy)

target_link_libraries(libfuzzy-dynamic -L${FLOWDESIGNER_SOURCE_DIR}/data-flow/src -llibflow ${LIBXML2_LIBRARIES})

install(TARGETS libfuzzy-dynamic DESTINATION bin)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/FuzzyEngine/src OUTPUT_FILE ${CMAKE_INSTALL_PREFIX}/libfuzzy.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${FUZZY_SRCS})" )  


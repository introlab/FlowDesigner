	
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
	${LIBXML2_INCLUDE} 
)
	
add_library(libfuzzy-dynamic SHARED ${FUZZY_SRCS})
	
set_target_properties(libfuzzy-dynamic PROPERTIES OUTPUT_NAME libfuzzy.tlb)

target_link_libraries(libfuzzy-dynamic libflow-dynamic ${LIBXML2_LIBRARIES})

install(TARGETS libfuzzy-dynamic DESTINATION ${FD_TOOLBOX_DIR}/FUZZY)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/FuzzyEngine/src OUTPUT_FILE libfuzzy.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${FUZZY_SRCS})" )  
INSTALL( FILES libfuzzy.def DESTINATION ${FD_TOOLBOX_DIR}/FUZZY/)

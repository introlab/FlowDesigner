#WHERE TO FIND XML FILES FOR TESTING
ADD_DEFINITIONS (-DTEST_DOCUMENT_DIR=\"${FLOWDESIGNER_SOURCE_DIR}/data-flow/gtest/n-files/\")
	
#WE SHOULD HAVE A GTEST FIND 
INCLUDE_DIRECTORIES(${GTEST_INCLUDES} ${LIBXML2_INCLUDE_DIR} ../include)
	
#ADD ALL TESTS HERE
SET(TEST_SRCS
    t_Matrix.cc           
    t_UINode.cc           
    t_UITerminal.cc
    t_UIDocument.cc       
    t_UINodeParameters.cc 
    t_Vector.cc
    t_UILink.cc           
    t_UINodeRepository.cc 
    t_UINetTerminal.cc    
    t_UINote.cc
    t_UINetwork.cc        
    t_UIProbeLink.cc
)

SET (TEST_HEADERS
	t_GenericUIFixture.h
)

#This is the single executable that will run all tests
add_executable(allTests ${TEST_SRCS} ${TEST_HEADERS})
target_link_libraries(allTests libflow-dynamic ${GTEST_LIBRARIES})




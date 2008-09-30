	
#WE SHOULD HAVE A GTEST FIND 
INCLUDE_DIRECTORIES(/opt/local/include ${LIBXML2_INCLUDE_DIR} ../include)
	
#ADD ALL TESTS HERE
SET(TEST_SRCS
	t_UIDocument.cc
)


#This is the single executable that will run all tests
add_executable(allTests ${TEST_SRCS})
target_link_libraries(allTests libflow-dynamic -L/opt/local/lib -lgtest -lgtest_main)




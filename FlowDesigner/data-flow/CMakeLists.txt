ADD_SUBDIRECTORY(src)
ADD_SUBDIRECTORY(include)
ADD_SUBDIRECTORY(n-files)

#Add gtest directory if gtest is found
IF (GTEST_FOUND)
#    ADD_SUBDIRECTORY(gtest)
ENDIF(GTEST_FOUND)

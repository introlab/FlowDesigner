
find_package(OpenCV REQUIRED)
	
SET(FDOPENCV_SRCS
   CvImage.cc 
   CvStructuringElement.cc 
   CvColor.cc 
   CvVideo.cc 
   CvContours.cc 
   CvAdd.cc
   CvCmp.cc
   CvAvg.cc
   CvGravityCenter.cc
   CvCreateColor.cc 
   CvFindColorRGB.cc 
   CvFindColorHSV.cc 
   CvContourArea.cc
   CvDrawContours.cc 
   CvFindContours.cc 
   CvFirstContour.cc
   CvPrevContour.cc 
   CvNextContour.cc 
   CvNextHierarchy.cc 
   CvPrevHierarchy.cc 
   CvSelectContour.cc 
   CvLine.cc 
   CvRectangle.cc 
   CvCreateImage.cc 
   CvSaveImage.cc 
   CvLoadImage.cc 
   CvShowImageQt.cc 
   CvLoadVideo.cc 
   CvWebcam.cc 
   CvOpticalFlowLK.cc 
   CvOpticalFlowHS.cc 
   CvQueryFrame.cc 
   CvCanny.cc 
   CvDilate.cc 
   CvErode.cc 
   CvGray.cc 
   CvMorphology.cc 
   CvSmooth.cc 
   CvSobel.cc 
   CvThreshold.cc 
   CvCreateStructuringElement.cc
   CvJitterMatrixToImage.cc  
   CvImageToJitterMatrix.cc
)	

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include 
     	./
	${LIBXML2_INCLUDE} 
	${OpenCV_INCLUDE_DIRS}
)
	
add_library(libopencv-dynamic SHARED ${FDOPENCV_SRCS})
	
set_target_properties(libopencv-dynamic PROPERTIES OUTPUT_NAME libopencv.tlb)

target_link_libraries(libopencv-dynamic -L${FLOWDESIGNER_SOURCE_DIR}/data-flow/src -llibflow ${LIBXML2_LIBRARIES}
	-LC:/PROGRA~1/OpenCV/bin -lcv100 -lcvaux100 -lcvcam100 -lcxcore100 -lcxts001 -lhighgui100 -lml100

)

install(TARGETS libopencv-dynamic DESTINATION ${FD_TOOLBOX_DIR}/FDOPENCV)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/FDOpenCV OUTPUT_FILE ${CMAKE_INSTALL_PREFIX}/${FD_TOOLBOX_DIR}/FDOPENCV/libopencv.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${FDOPENCV_SRCS})" )  


find_package(OpenCV REQUIRED)

SET(FDOPENCV_DLL
 "C:/Program Files/OpenCV/bin/highgui100.dll"
 "C:/Program Files/OpenCV/bin/cxcore100.dll"
 "C:/Program Files/OpenCV/bin/cv100.dll"
 "C:/Program Files/OpenCV/bin/cvcam100.dll"
 "C:/Program Files/OpenCV/bin/cvaux100.dll"
 "C:/Program Files/OpenCV/bin/cxts001.dll"
 "C:/Program Files/OpenCV/bin/libguide40.dll"
 "C:/Program Files/OpenCV/bin/ml100.dll"
 )
 
SET(FDOPENCV_EXAMPLES
 example/azimut.jpg
 example/building.jpg
 example/findContourRect.n
 example/rectangle.bmp
 example/selectContour.n
 example/Transformation.n
 example/webcamFindColor.n
 example/webcamFindMouvement.n
 example/webcamGravityCenter.n
 )

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
	
set_target_properties(libopencv-dynamic PROPERTIES OUTPUT_NAME opencv.tlb)

#target_link_libraries(libopencv-dynamic libflow-dynamic ${LIBXML2_LIBRARIES}
#	-LC:/PROGRA~1/OpenCV/bin -lcv100 -lcvaux100 -lcvcam100 -lcxcore100 -lcxts001 -lhighgui100 -lml100
target_link_libraries(libopencv-dynamic libflow-dynamic ${LIBXML2_LIBRARIES}
	-LC:/PROGRA~1/OpenCV/bin -lcv -lcvaux -lcxcore -lhighgui -lml

)

install(TARGETS libopencv-dynamic DESTINATION ${FD_TOOLBOX_DIR}/FDOPENCV/lib)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/toolbox/FDOpenCV OUTPUT_FILE libopencv.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${FDOPENCV_SRCS})" )  
INSTALL( FILES libopencv.def DESTINATION ${FD_TOOLBOX_DIR}/FDOPENCV/def)

#Install OpenCV Dlls
IF (WIN32)
	INSTALL( FILES ${FDOPENCV_DLL} DESTINATION bin)
ENDIF(WIN32)

#Install examples
#INSTALL( FILES ${FDOPENCV_EXAMPLES} DESTINATION ${FD_PREFIX}/shared/flowdesigner/FDOPENCV)

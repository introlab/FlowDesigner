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

SET(RFOPENCV_SRCS
    Image.cc
	ColorHistExtraction.cc
	CvCaptureFrame.cc
	CvFaceDetection.cc
	CvHaarDetect.cc
	DrawPFParticle.cc
	GetVisualROIParam.cc
	IntegralColorExtraction.cc
	IntegralEdgesOriExtraction.cc
	IntegralLBPExtraction.cc
	Makefile.am
	MeanShiftTracker.cc
	MultiIntegralCuesPFTracker.cc
	PFGenericParticle.cc
	PFGenericParticleFilter.cc
	PFPMRandomWalk.cc
	PFState2VisualROI.cc
	PFUtilityFct.cc
	TextSignDetect.cc
	VisualFeatureDesc.cc
	VisualHistogramDesc.cc
	VisualIntegralDesc.cc
	VisualROI.cc
	VisualTarget.cc
	VisualTargetManager.cc
	VisualTargetPFInterface.cc
)

SET(ADDITIONAL_INCLUDES
	"C:/Dev-Cpp/include"
)

SET(ADDITIONAL_LIBS
	"-LC:/Dev-Cpp/lib -ljpeg"
)

INCLUDE_DIRECTORIES(${FLOWDESIGNER_SOURCE_DIR}/data-flow/include
	./
	${LIBXML2_INCLUDE} 
	${OpenCV_INCLUDE_DIRS}
	${ADDITIONAL_INCLUDES}
)
	
add_library(libopencv-dynamic SHARED ${FDOPENCV_SRCS} ${RFOPENCV_SRCS})
	
set_target_properties(libopencv-dynamic PROPERTIES OUTPUT_NAME opencv.tlb)

#target_link_libraries(libopencv-dynamic libflow-dynamic ${LIBXML2_LIBRARIES}
#	-LC:/PROGRA~1/OpenCV/bin -lcv100 -lcvaux100 -lcvcam100 -lcxcore100 -lcxts001 -lhighgui100 -lml100

target_link_libraries(libopencv-dynamic libflow-dynamic ${LIBXML2_LIBRARIES} ${OpenCV_LIBRARIES} ${ADDITIONAL_LIBS})


install(TARGETS libopencv-dynamic DESTINATION ${FD_TOOLBOX_DIR}/FDOPENCV/lib)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FLOWDESIGNER_SOURCE_DIR}/toolbox/FDOpenCV OUTPUT_FILE libopencv.def COMMAND ${PERL} ${FLOWDESIGNER_SOURCE_DIR}/tools/src/info2def.pl ${FDOPENCV_SRCS} ${RFOPENCV_SRCS})" )  
INSTALL( FILES libopencv.def DESTINATION ${FD_TOOLBOX_DIR}/FDOPENCV/def)

#Install OpenCV Dlls
IF (WIN32)
	INSTALL( FILES ${FDOPENCV_DLL} DESTINATION bin)
	INSTALL( FILES "C:/Dev-Cpp/bin/jpeg62.dll" DESTINATION bin)
ENDIF(WIN32)

#Install examples
#INSTALL( FILES ${FDOPENCV_EXAMPLES} DESTINATION ${FD_PREFIX}/shared/flowdesigner/FDOPENCV)

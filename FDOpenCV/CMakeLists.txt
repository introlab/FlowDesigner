PROJECT (FDOpenCV)

#Look for minimum cmake version
cmake_minimum_required(VERSION 2.4.0)

SET(CMAKE_BUILD_TYPE Release )
SET(CMAKE_VERBOSE_MAKEFILE ON)

IF (WIN32)
	SET(FD_PREFIX "C:/Progra~1/FlowDesigner")
ELSE(WIN32)
	SET(FD_PREFIX "/usr")
ENDIF(WIN32)
	
SET (FLOWDESIGNER_BIN_DIR "${FD_PREFIX}/bin/")
SET (FLOWDESIGNER_INCLUDE_DIR "${FD_PREFIX}/include/flowdesigner/")
SET (FLOWDESIGNER_LIB_DIR "${FD_PREFIX}/lib/")
SET (FLOWDESIGNER_TOOLBOX_DIR "${FD_PREFIX}/lib/flowdesigner/toolbox/")


#Add the macros directory for cmake scripts
SET(CMAKE_MODULE_PATH "${FDOpenCV_SOURCE_DIR}/macros")

find_package(OpenCV REQUIRED)
find_package(LibXml2 REQUIRED)
find_package(Perl REQUIRED)

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

#set the prefix
SET(CMAKE_INSTALL_PREFIX ${FD_PREFIX})

INCLUDE_DIRECTORIES(${FLOWDESIGNER_INCLUDE_DIR}
     	./
	${LIBXML2_INCLUDE} 
	${OpenCV_INCLUDE_DIRS}
)
	
add_library(libopencv-dynamic SHARED ${FDOPENCV_SRCS})
	
set_target_properties(libopencv-dynamic PROPERTIES OUTPUT_NAME libopencv.tlb)

target_link_libraries(libopencv-dynamic -L${FLOWDESIGNER_LIB_DIR} -llibflow ${LIBXML2_LIBRARIES}
	-LC:/PROGRA~1/OpenCV/bin -lcv100 -lcvaux100 -lcvcam100 -lcxcore100 -lcxts001 -lhighgui100 -lml100

)

install(TARGETS libopencv-dynamic DESTINATION ${FLOWDESIGNER_TOOLBOX_DIR}/FDOPENCV/lib)
  
#INSTALLING .def file  
INSTALL( CODE "EXECUTE_PROCESS(WORKING_DIRECTORY ${FDOpenCV_SOURCE_DIR} OUTPUT_FILE libopencv.def COMMAND ${PERL} ${FLOWDESIGNER_BIN_DIR}/info2def.pl ${FDOPENCV_SRCS})" )  
INSTALL( FILES libopencv.def DESTINATION ${FLOWDESIGNER_TOOLBOX_DIR}/FDOPENCV/def)

#Install OpenCV Dlls
IF (WIN32)
	INSTALL( FILES ${FDOPENCV_DLL} DESTINATION bin)
ENDIF(WIN32)

#Install examples
INSTALL( FILES ${FDOPENCV_EXAMPLES} DESTINATION ${FD_PREFIX}/shared/flowdesigner/FDOPENCV)

SET(CPACK_PACKAGE_NAME "FDOpenCV")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "OpenCV FlowDesigner Toolbox")
SET(CPACK_PACKAGE_VENDOR "Dominic Letourneau, Julien D'Ascenzio")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")

IF(APPLE)
	SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/license.txt")
ELSE(APPLE)
	SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING")
ENDIF(APPLE)

SET(CPACK_PACKAGE_VERSION_MAJOR "0")
SET(CPACK_PACKAGE_VERSION_MINOR "1")
SET(CPACK_PACKAGE_VERSION_PATCH "0")
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "FlowDesigner")

IF(WIN32 AND NOT UNIX)
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  # SET(CPACK_PACKAGE_ICON "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
  # SET(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\FDSetup.exe")
  SET(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} FDOpenCV Toolkit")
  # SET(CPACK_NSIS_HELP_LINK "http:\\\\\\\\manyears.sourceforge.net")
  SET(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\flowdesigner.sourceforge.net")
  SET(CPACK_NSIS_CONTACT "flowdesigner-devel@lists.sourceforge.net")
  SET(CPACK_NSIS_MODIFY_PATH ON)
  #SET(CPACK_PACKAGE_EXECUTABLES "")
  SET(CPACK_TOPLEVEL_TAG "FDOpenCV")
ENDIF(WIN32 AND NOT UNIX)

INCLUDE(CPack)

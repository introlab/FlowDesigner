## Process this file with automake to produce Makefile.in. -*-Makefile-*-
# $Id: Makefile.am,v 1.38 2005/07/28 15:09:19 maestro Exp $
# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libFDOpenCV.la

# Sources for compilation in the library
libFDOpenCV_la_SOURCES =  CvImage.cc \
   CvStructuringElement.cc \
   CvColor.cc \
   CvVideo.cc \
   CvLoadVideo.cc \
   CvQueryFrame.cc \
   CvContours.cc \
   CvSelectContour.cc \
   CvFindContours.cc \
   CvContourArea.cc\
   CvFindColorRGB.cc \
   CvFindColorHSV.cc \
   CvDrawContours.cc \
   CvCreateStructuringElement.cc \
   CvCreateImage.cc \
   CvRectangle.cc \
	CvShowImage.cc \
	CvLoadImage.cc \
   CvSaveImage.cc \
   CvCanny.cc \
   CvGray.cc \
   CvThreshold.cc \
   CvErode.cc \
   CvDilate.cc \
   CvSobel.cc \
   CvSmooth.cc \   
   CvCreateColor.cc \
   CvAdd.cc\
   CvCmp.cc\
   CvMorphology.cc \
   CvPrevContour.cc \ 
   CvNextContour.cc \
   CvFirstContour.cc \
   CvNextHierarchy.cc \
   CvPrevHierarchy.cc \
   CvOpticalFlow.cc \
   CvLine.cc
   
libFDOpenCV_la_LDFLAGS = -release $(LT_RELEASE) 

libFDOpenCV_la_LIBADD = $(OPENCV_LIBS) $(GDK_LIBS)

INCLUDES = -I../include $(FLOWDESIGNER_INCLUDE) $(OPENCV_CFLAGS) $(GDK_CFLAGS)

install-data-local:
	echo "Installing $(PACKAGE) in $(libdir)"
	mkdir -p $(libdir)
	(perl $(FLOWDESIGNER_BIN)/info2def.pl $(libFDOpenCV_la_SOURCES) > $(prefix)/libFDOpenCV.def)
	(rm -f $(libdir)/libFDOpenCV.tlb; cd $(libdir); ln -s libFDOpenCV.so libFDOpenCV.tlb)

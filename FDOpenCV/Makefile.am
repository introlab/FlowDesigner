## Process this file with automake to produce Makefile.in. -*-Makefile-*-
# $Id: Makefile.am,v 1.1 2007/06/29 14:28:21 jdascenzio Exp $
# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libFDOpenCV.la

# Sources for compilation in the library
libFDOpenCV_la_SOURCES =  \
   CvImage.cc \
   CvStructuringElement.cc \
   CvColor.cc \
   CvVideo.cc \
   CvContours.cc \
   CvAdd.cc\
   CvCmp.cc\
   CvCreateColor.cc \
   CvFindColorRGB.cc \
   CvFindColorHSV.cc \
   CvContourArea.cc\
   CvDrawContours.cc \
   CvFindContours.cc \
   CvFirstContour.cc \
   CvPrevContour.cc \
   CvNextContour.cc \
   CvNextHierarchy.cc \
   CvPrevHierarchy.cc \
   CvSelectContour.cc \
   CvLine.cc \
   CvRectangle.cc \
   CvCreateImage.cc \
   CvSaveImage.cc \
	CvLoadImage.cc \
	CvShowImage.cc \
   CvLoadVideo.cc \
   CvOpticalFlow.cc \
   CvQueryFrame.cc \
   CvCanny.cc \
   CvDilate.cc \
   CvErode.cc \
   CvGray.cc \
   CvMorphology.cc \
   CvSmooth.cc \
   CvSobel.cc \
   CvThreshold.cc \
   CvCreateStructuringElement.cc

libFDOpenCV_la_LDFLAGS = -release $(LT_RELEASE) 

libFDOpenCV_la_LIBADD = $(OPENCV_LIBS) $(GDK_LIBS)

INCLUDES = -I../include $(FLOWDESIGNER_INCLUDE) $(OPENCV_CFLAGS) $(GDK_CFLAGS)

install-data-local:
	echo "Installing $(PACKAGE) in $(libdir)"
	mkdir -p $(libdir)
	(perl $(FLOWDESIGNER_BIN)/info2def.pl $(libFDOpenCV_la_SOURCES) > $(prefix)/libFDOpenCV.def)
	(rm -f $(libdir)/libFDOpenCV.tlb; cd $(libdir); ln -s libFDOpenCV.so libFDOpenCV.tlb)

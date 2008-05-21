## Process this file with automake to produce Makefile.in. -*-Makefile-*-
# $Id: Makefile.am,v 1.5 2007/07/11 20:04:26 jdascenzio Exp $
# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

SUBDIRS = examples

override includedir=${prefix}/lib/flowdesigner/toolbox/FDOpenCV/include
override datadir=${prefix}/lib/flowdesigner/toolbox/FDOpenCV/def
override libdir=${prefix}/lib/flowdesigner/toolbox/FDOpenCV/lib

lib_LTLIBRARIES = libFDOpenCV.la


noinst_HEADERS = CvColor.h \
    CvContours.h \
    CvImage.h \
    CvStructuringElement.h \
    CvVideo.h


# Sources for compilation in the library
libFDOpenCV_la_SOURCES =  CvImage.cc \
	CvStructuringElement.cc \
	CvColor.cc \
	CvVideo.cc \
	CvContours.cc \
	CvAdd.cc\
	CvCmp.cc\
	CvAvg.cc\
	CvGravityCenter.cc\
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
	CvWebcam.cc \
	CvOpticalFlowLK.cc \
	CvOpticalFlowHS.cc \
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


EXTRA_DIST = CMakeLists.txt COPYING FDOpenCV.spec

libFDOpenCV_la_LDFLAGS = -release $(LT_RELEASE) 

libFDOpenCV_la_LIBADD = $(OPENCV_LIBS) $(GDK_LIBS)

INCLUDES = -I../include $(FLOWDESIGNER_INCLUDE) $(OPENCV_CFLAGS) $(GDK_CFLAGS)

install-data-local:
	echo "Installing $(PACKAGE) in $(libdir)"
	mkdir -p $(libdir)
	mkdir -p $(datadir)
	(perl $(FLOWDESIGNER_BIN)/info2def.pl $(libFDOpenCV_la_SOURCES) > $(datadir)/libFDOpenCV.def)
	(rm -f $(libdir)/libFDOpenCV.tlb; cd $(libdir); ln -s libFDOpenCV.so libFDOpenCV.tlb)	




	

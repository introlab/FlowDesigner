## Process this file with automake to produce Makefile.in

INCLUDES = \
	-I$(top_srcdir)/intl \
	$(GNOME_INCLUDEDIR) \
	$(GNOME_XML_INCLUDE) \
	$(GNOME_GTHR_INCLUDE) \
	-I$(top_srcdir)/../data-flow/include

lib_LTLIBRARIES = libflowgui.la libflowui.la 

libflowui_la_SOURCES = \
	UINode.h UINode.cc \
	UINetwork.h UINetwork.cc \
	UIDocument.h UIDocument.cc \
	UITerminal.h UITerminal.cc \
	UILink.h UILink.cc \
	UINodeParameters.h UINodeParameters.cc \
	UINetTerminal.h UINetTerminal.cc

libflowgui_la_SOURCES = \
	GUINode.h GUINode.cc \
	GUINetwork.h GUINetwork.cc \
	GUIDocument.h GUIDocument.cc \
	GUITerminal.h GUITerminal.cc \
	GUILink.h GUILink.cc \
	GUINodeParameters.h GUINodeParameters.cc \
	GUINetTerminal.h GUINetTerminal.cc \
	GUINetPopup.h GUINetPopup.cc


bin_PROGRAMS = vflow batchflow

vflow_SOURCES = \
	main.cc \
	Probe.cc Probe.h \
	TextProbe.cc TextProbe.h \
	PlotProbe.cc PlotProbe.h \
	SpectrumProbe.cc \
	canvas-background.c canvas-background.h

batchflow_SOURCES = \
	batchflow.cc

vflow_LDADD = $(GNOME_LIBDIR) $(GNOMEUI_LIBS) $(INTLLIBS) $(GNOME_XML_LIB) $(GNOME_GTHR_LIB) \
	$(top_srcdir)/../data-flow/src/libnetwork.la ./libflowui.la ./libflowgui.la\
	$(LIB_FFTW)

batchflow_LDADD = $(INTLLIBS) $(GNOME_XML_LIB) $(GNOME_GTHR_LIB) \
	$(top_srcdir)/../data-flow/src/libnetwork.la ./libflowui.la\
	$(LIB_FFTW)

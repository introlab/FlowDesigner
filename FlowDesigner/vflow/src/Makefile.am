## Process this file with automake to produce Makefile.in

install:
	mkdir -p $(prefix)/toolbox/vflow
	perl $(top_srcdir)/../tools/src/info2def.pl $(top_srcdir)/src/* > $(prefix)/toolbox/vflow/vflow.def

INCLUDES = \
	-I$(top_srcdir)/intl \
	$(GNOME_INCLUDEDIR) \
	$(GNOME_XML_INCLUDE) \
	$(GNOME_GTHR_INCLUDE) \
	-I$(top_srcdir)/../data-flow/include \
	-I$(top_srcdir)/include

lib_LTLIBRARIES = libflowgui.la libflowui.la 

libflowui_la_SOURCES = \
	UINode.cc \
	UINetwork.cc \
	UIDocument.cc \
	UITerminal.cc \
	UILink.cc \
	UINodeParameters.cc \
	UINetTerminal.cc

libflowgui_la_SOURCES = \
	GUINode.cc \
	GUINetwork.cc \
	GUIDocument.cc \
	GUITerminal.cc \
	GUILink.cc \
	GUINodeParameters.cc \
	GUINetTerminal.cc \
	GUINetPopup.cc


bin_PROGRAMS = vflow batchflow

vflow_SOURCES = \
	main.cc \
	Probe.cc \
	TextProbe.cc \
	PlotProbe.cc \
	SpectrumProbe.cc \
	canvas-background.c \
	GUINode.cc \
	GUINetwork.cc \
	GUIDocument.cc \
	GUITerminal.cc \
	GUILink.cc \
	GUINodeParameters.cc \
	GUINetTerminal.cc \
	GUINetPopup.cc

batchflow_SOURCES = \
	batchflow.cc

vflow_LDADD = $(GNOME_LIBDIR) $(GNOMEUI_LIBS) $(INTLLIBS) $(GNOME_XML_LIB) $(GNOME_GTHR_LIB) \
	$(top_srcdir)/../data-flow/src/libnetwork.la ./libflowui.la \
	$(LIB_FFTW)

batchflow_LDADD = $(INTLLIBS) $(GNOME_XML_LIB) $(GNOME_GTHR_LIB) \
	$(top_srcdir)/../data-flow/src/libnetwork.la ./libflowui.la\
	$(LIB_FFTW)

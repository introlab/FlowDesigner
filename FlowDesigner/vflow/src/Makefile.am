## Process this file with automake to produce Makefile.in

install:
	mkdir -p $(prefix)/toolbox/vflow
	perl $(top_srcdir)/../tools/src/info2def.pl $(top_srcdir)/src/*.cc > $(prefix)/toolbox/vflow/vflow.def

INCLUDES = \
	-I$(top_srcdir)/intl \
	$(GNOME_INCLUDEDIR) \
	$(GNOME_XML_INCLUDE) \
	$(FFTW_INCLUDES) \
	$(GNOME_GTHR_INCLUDE) \
	-I$(top_srcdir)/../data-flow/include \
	-I$(top_srcdir)/../audio_blocks/include \
	-I$(top_srcdir)/include

bin_PROGRAMS = vflow

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

LDFLAGS = $(FFTW_LDFLAGS)

vflow_LDADD = $(GNOME_LIBDIR) $(GNOMEUI_LIBS) $(INTLLIBS) $(GNOME_XML_LIB) $(GNOME_GTHR_LIB) \
	$(top_srcdir)/../data-flow/src/libflow.la \
	$(LIB_FFTW)

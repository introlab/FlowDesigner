## Process this file with automake to produce Makefile.in

override includedir=${prefix}/include/flowdesigner
override datadir=${prefix}/lib/flowdesigner/toolbox/vflow
override libdir=${prefix}/lib/flowdesigner/toolbox/vflow

install-data-local:
	mkdir -p $(datadir)
	(cd $(top_srcdir); perl ../tools/src/info2def.pl src/*.cc include/*.h> $(datadir)/vflow.def)
	(cd $(libdir); rm -f baseProbe.tlb; ln -s libbaseProbe.so baseProbe.tlb)

INCLUDES = \
           -DPACKAGE_DATA_DIR=\""$(datadir)"\" \
           -DPACKAGE_LOCALE_DIR=\""$(prefix)/$(DATADIRNAME)/locale"\" \
           @PACKAGE_CFLAGS@ \
	$(GNOME_XML_INCLUDE) \
	$(FFTW_INCLUDES) \
	$(GNOME_GTHR_INCLUDE) \
	$(GNOME_PIXBUF_INCLUDE) \
	-I$(top_srcdir)/../data-flow/include \
	-I$(top_srcdir)/../audio_blocks/include \
	-I$(top_srcdir)/include


bin_PROGRAMS = flowdesigner gflow

lib_LTLIBRARIES = libbaseProbe.la 

flowdesigner_SOURCES = vflow.cc \
	canvas-background.c \
	GUINode.cc \
	GUINetwork.cc \
	GUIDocument.cc \
	GUITerminal.cc \
	GUILink.cc \
	GUINodeParameters.cc \
	GUINetTerminal.cc \
	GUINetPopup.cc \
	GRunContext.cc \
	codegen.cc \
	GUINodeTooltip.cc \
	vflow_pref.cc \
	misc_gui.cc

gflow_SOURCES = gflow.cc \
	GRunContext.cc \
	vflow_pref.cc 

libbaseProbe_la_SOURCES = Probe.cc \
	TextProbe.cc \
	PlotProbe.cc \
	GtkPlotProbe.cc \
	KeyPad.cc

libbaseProbe_la_LDFLAGS = -release $(LT_RELEASE)


LDFLAGS = $(FFTW_LDFLAGS)

LDADD = @PACKAGE_LIBS@ $(INTLLIBS) $(GNOME_XML_LIB) $(GNOME_GTHR_LIB) \
	$(top_srcdir)/../data-flow/src/libflow.la \
	$(LIB_FFTW) $(GNOME_PIXBUF_LIB)

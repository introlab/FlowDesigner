## Process this file with automake to produce Makefile.in

override includedir=${prefix}/include/flowdesigner
override datadir=${prefix}/lib/flowdesigner/toolbox/vflow
override libdir=${prefix}/lib/flowdesigner/toolbox/vflow
#default bindir is OK


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
	misc_gui.cc \
	GUINote.cc \
	GUITreeView.cc

gflow_SOURCES = gflow.cc \
	GRunContext.cc  

libbaseProbe_la_SOURCES = Probe.cc \
	TextProbe.cc \
	PlotProbe.cc \
	GtkPlotProbe.cc \
	KeyPad.cc

install-data-local:
	mkdir -p $(datadir)
if WITH_CYGWIN		
	(cd $(libdir); rm -f baseProbe.tlb; \
	ln -s $(prefix)/lib/flowdesigner/toolbox/bin/cygbaseProbe-$(FLOWDESIGNER_MAJOR_VERSION)-$(FLOWDESIGNER_MINOR_VERSION).dll baseProbe.tlb)
else
	(cd $(libdir); rm -f baseProbe.tlb; ln -s libbaseProbe.so baseProbe.tlb)	
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(libbaseProbe_la_SOURCES) > $(datadir)/vflow.def)


libbaseProbe_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS) $(top_srcdir)/../data-flow/src/libflow.la \
	$(GNOME_XML_LIB) $(GNOME_GTHR_LIB) $(LIB_FFTW) $(GNOME_PIXBUF_LIB)


LDFLAGS = $(FFTW_LDFLAGS) $(CYGWIN_LDFLAGS)

LDADD = @PACKAGE_LIBS@ $(GNOME_XML_LIB) $(GNOME_GTHR_LIB) \
	$(top_srcdir)/../data-flow/src/libflow.la \
	$(LIB_FFTW) $(GNOME_PIXBUF_LIB)

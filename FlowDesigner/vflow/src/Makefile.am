## Process this file with automake to produce Makefile.in

INCLUDES = \
	-I$(top_srcdir)/intl \
	$(GNOME_INCLUDEDIR) \
	$(GNOME_XML_INCLUDE) \
	-I$(top_srcdir)/../data-flow/include

bin_PROGRAMS = vflow

vflow_SOURCES = \
	main.cc \
	UINode.h UINode.cc \
	UINetwork.h UINetwork.cc \
	UIDocument.h UIDocument.cc \
	UITerminal.h UITerminal.cc \
	UILink.h UILink.cc \
	UINodeParameters.h UINodeParameters.cc \
	UINetPopup.h UINetPopup.cc \
	UINetTerminal.h UINetTerminal.cc \
	GUINode.h GUINode.cc \
	GUINetwork.h GUINetwork.cc \
	GUIDocument.h GUIDocument.cc \
	GUITerminal.h GUITerminal.cc \
	GUILink.h GUILink.cc \
	GUINodeParameters.h GUINodeParameters.cc \
	GUINetTerminal.h GUINetTerminal.cc 


vflow_LDADD = $(GNOME_LIBDIR) $(GNOMEUI_LIBS) $(INTLLIBS) $(GNOME_XML_LIB) \
	$(top_srcdir)/../audio_blocks/src/libablocks.la \
	$(top_srcdir)/../HMM/src/libhmm.la \
	$(top_srcdir)/../VQ/src/libvq.la \
	$(top_srcdir)/../data-flow/src/libnetwork.la \
	$(LIB_FFTW)


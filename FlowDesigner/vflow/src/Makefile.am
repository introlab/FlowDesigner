## Process this file with automake to produce Makefile.in

INCLUDES = \
	-I$(top_srcdir)/intl \
	$(GNOME_INCLUDEDIR) \
	$(GNOME_XML_INCLUDE)

bin_PROGRAMS = vflow

vflow_SOURCES = \
	main.cc \
	UINode.h UINode.cc \
	UINetwork.h UINetwork.cc \
	UIDocument.h UIDocument.cc \
	UITerminal.h UITerminal.cc \
	UILink.h UILink.cc \
	UINodeParameters.h UINodeParameters.cc \
	UINetPopup.h UINetPopup.cc

vflow_LDADD = $(GNOME_LIBDIR) $(GNOMEUI_LIBS) $(INTLLIBS) $(GNOME_XML_LIB)


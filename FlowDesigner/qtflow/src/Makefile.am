## Process this file with automake to produce Makefile.in

override includedir=${prefix}/include/flowdesigner
override datadir=${prefix}/lib/flowdesigner/toolbox/qtflow
override libdir=${prefix}/lib/flowdesigner/toolbox/qtflow
#default bindir is OK


INCLUDES = @PACKAGE_CFLAGS@ \
	$(GNOME_PIXBUF_INCLUDE) \
	-I$(top_srcdir)/../data-flow/include  \
	-I../include


bin_PROGRAMS = flowdesigner-qt qtflow

lib_LTLIBRARIES = libbaseProbeQt.la 

flowdesigner_qt_SOURCES = main.cc \
	QtLink.cc  \
	.moc/moc_QtLink.cc \
	QtNetwork.cc  \
	.moc/moc_QtNetwork.cc \
	QtNode.cc  \
	.moc/moc_QtNode.cc \
	QtTerminal.cc \
	.moc/moc_QtTerminal.cc \
	QtFlowDesigner.cc \
	.moc/moc_QtFlowDesigner.cc \
	QtNodeTreeView.cc \
	.moc/moc_QtNodeTreeView.cc \
	QtDocument.cc \
	.moc/moc_QtDocument.cc \
	QtNetTerminal.cc \
	.moc/moc_QtNetTerminal.cc \
	QtNodeParameters.cc \
	.moc/moc_QtNodeParameters.cc \
	QtDLManager.cc \
	.moc/moc_QtDLManager.cc \
	QtRunContext.cc \
	.moc/moc_QtRunContext.cc \
	UILinkController.cc \
	.moc/moc_UILinkController.cc \
	UINodeController.cc \
	.moc/moc_UINodeController.cc \
	UINetTerminalController.cc \
	.moc/moc_UINetTerminalController.cc \
	UITerminalController.cc \
	.moc/moc_UITerminalController.cc \
	UIDocumentController.cc \
	.moc/moc_UIDocumentController.cc \
	UINetworkController.cc \
	.moc/moc_UINetworkController.cc

qtflow_SOURCES = qtflow.cc \
	QtLink.cc  \
	.moc/moc_QtLink.cc \
	QtNetwork.cc  \
	.moc/moc_QtNetwork.cc \
	QtNode.cc  \
	.moc/moc_QtNode.cc \
	QtTerminal.cc \
	.moc/moc_QtTerminal.cc \
	QtFlowDesigner.cc \
	.moc/moc_QtFlowDesigner.cc \
	QtNodeTreeView.cc \
	.moc/moc_QtNodeTreeView.cc \
	QtDocument.cc \
	.moc/moc_QtDocument.cc \
	QtNetTerminal.cc \
	.moc/moc_QtNetTerminal.cc \
	QtNodeParameters.cc \
	.moc/moc_QtNodeParameters.cc \
	QtDLManager.cc \
	.moc/moc_QtDLManager.cc \
	QtRunContext.cc \
	.moc/moc_QtRunContext.cc \
	UILinkController.cc \
	.moc/moc_UILinkController.cc \
	UINodeController.cc \
	.moc/moc_UINodeController.cc \
	UINetTerminalController.cc \
	.moc/moc_UINetTerminalController.cc \
	UITerminalController.cc \
	.moc/moc_UITerminalController.cc \
	UIDocumentController.cc \
	.moc/moc_UIDocumentController.cc \
	UINetworkController.cc \
	.moc/moc_UINetworkController.cc


libbaseProbeQt_la_SOURCES = empty.cc 

install-data-local:
	mkdir -p $(datadir)
if WITH_CYGWIN		
	(cd $(libdir); rm -f baseProbeQt.tlb; \
	ln -s $(prefix)/lib/flowdesigner/toolbox/bin/cygbaseProbeQt-$(FLOWDESIGNER_MAJOR_VERSION)-$(FLOWDESIGNER_MINOR_VERSION).dll baseProbeQt.tlb)
else
	(cd $(libdir); rm -f baseProbeQt.tlb; ln -s libbaseProbeQt.so baseProbeQt.tlb)	
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(libbaseProbeQt_la_SOURCES) > $(datadir)/qtflow.def)


libbaseProbeQt_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS) \
	$(top_srcdir)/../data-flow/src/libflow.la \
	$(PACKAGE_CFLAGS) 


flowdesigner_qt_LDFLAGS = $(FFTW_LDFLAGS) $(CYGWIN_LDFLAGS) $(PACKAGE_LIBS) $(top_srcdir)/../data-flow/src/libflow.la

qtflow_LDFLAGS = $(FFTW_LDFLAGS) $(CYGWIN_LDFLAGS) $(PACKAGE_LIBS) $(top_srcdir)/../data-flow/src/libflow.la


# This rule lets GNU make create any moc_*.cc from the equivalent *.h
.moc/moc_%.cc: ../include/%.h
	mkdir -p .moc
	moc-qt4 $< -o $@

%.ui.cc: %.ui
	uic-qt4 $< -o $<.h
#	uic-qt4 -impl $<.h $< -o $@

# Adjust this line according to your resources.qrc
# resources.cc : resources.qrc octave-gui.png
# 	rcc resources.qrc -o resources.cc
#
# This line ensures that generated moc_*.cc and resources.cc files are
# deleted when we make clean.
CLEANFILES = $(filter .moc/moc_%.cc,$(flowdesigner_qt_SOURCES) $(libbaseProbeQt_la_SOURCES))

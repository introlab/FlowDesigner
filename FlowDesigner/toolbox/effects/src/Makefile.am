override includedir=${prefix}/lib/flowdesigner/toolbox/effects
override datadir=${prefix}/lib/flowdesigner/toolbox/effects
override libdir=${prefix}/lib/flowdesigner/toolbox/effects
override bindir=${prefix}/lib/flowdesigner/toolbox/effects

## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.20 2005/11/07 18:00:00 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libeffects.la 

libeffects_la_SOURCES =	Saturate.cc rev.cc Reverb.cc

install-data-local:
	mkdir -p $(datadir)

if WITH_CYGWIN
	(cd $(libdir); rm -f effects.tlb; \
	ln -s $(prefix)/lib/flowdesigner/toolbox/bin/cygeffects-$(FLOWDESIGNER_MAJOR_VERSION)-$(FLOWDESIGNER_MINOR_VERSION).dll effects.tlb)
else
	(cd $(libdir); rm -f effects.tlb; ln -s libeffects.so effects.tlb)	
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(libeffects_la_SOURCES) > $(datadir)/effects.def)


libeffects_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS) $(top_srcdir)/../data-flow/src/libflow.la

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include 

LDADD = $(top_srcdir)/../data-flow/src/libflow.la 



## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.2 2000/06/30 04:08:01 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libeffects.la 

install:
	(cd $(prefix)/lib; rm -f effects.tlb; ln -s libeffects.so effects.tlb)
	mkdir -p $(prefix)/toolbox/effects
	$(top_srcdir)/../tools/src/scan_toolbox.sh $(top_srcdir)/../tools/src/info2def.pl  $(top_srcdir)/src $(prefix)/toolbox/effects

libeffects_la_SOURCES =	Saturate.cc

libeffects_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/../data-flow/include -I$(top_srcdir)/include $(FFTW_INCLUDES)

LDADD = $(top_srcdir)/../data-flow/src/libnetwork.la \
	$(LIB_FFTW)



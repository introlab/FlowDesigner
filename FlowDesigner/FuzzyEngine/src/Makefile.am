## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.5 2001/02/22 03:20:38 jmvalin Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libfuzzy.la

install:
	(cd $(prefix)/lib; rm -f fuzzy.tlb; ln -s libfuzzy.so fuzzy.tlb)
	mkdir -p $(prefix)/toolbox/fuzzy
	perl $(top_srcdir)/../tools/src/info2def.pl $(top_srcdir)/src/*.cc > $(prefix)/toolbox/fuzzy/fuzzy.def

# Sources for compilation in the library
libfuzzy_la_SOURCES = FuzzyFunction.cc \
	FuzzyModel.cc \
	FuzzyRule.cc \
	FuzzySet.cc \
	GenericModel.cc \
	TrapezoidalFunction.cc \
	TriangularFunction.cc \
	FuzzyOperators.cc

#libvq_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) @LIBS@
libfuzzy_la_LDFLAGS = -release $(LT_RELEASE)

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

noinst_PROGRAMS = FuzzyTest 
FuzzyTest_SOURCES = FuzzyTest.cc

LDADD = $(top_srcdir)/src/libfuzzy.la \
        $(top_srcdir)/../data-flow/src/libflow.la $(GNOME_XML_LIB)

#ifdef FREEBSD
FREEBSD_FLAGS = -pthread
#endif

LDFLAGS = $(FREEBSD_FLAGS)

override includedir=${prefix}/lib/flowdesigner/toolbox/FuzzyEngine
override datadir=${prefix}/lib/flowdesigner/toolbox/FuzzyEngine
override libdir=${prefix}/lib/flowdesigner/toolbox/FuzzyEngine

## Process this file with automake to produce Makefile.in. -*-Makefile-*-

# $Id: Makefile.am,v 1.24 2005/11/01 21:45:32 maestro Exp $

# Disable automatic dependency tracking if using other tools than gcc and gmake
#AUTOMAKE_OPTIONS = no-dependencies

lib_LTLIBRARIES = libfuzzy.la


# Sources for compilation in the library
libfuzzy_la_SOURCES = FuzzyFunction.cc \
	FuzzyModel.cc \
	FuzzyRule.cc \
	FuzzySet.cc \
	GenericModel.cc \
	TrapezoidalFunction.cc \
	TriangularFunction.cc \
	FuzzyOperators.cc \
	FuzzyModelExec.cc \
	Tokenizer.cc \
	InferenceModel.cc


install-data-local:
	mkdir -p $(datadir)
if WITH_CYGWIN
	(cd $(libdir); rm -f fuzzy.tlb; ln -s libfuzzy.dll.a fuzzy.tlb)	
else
	(cd $(libdir); rm -f fuzzy.tlb; ln -s libfuzzy.so fuzzy.tlb)
endif
	(perl $(top_srcdir)/../tools/src/info2def.pl $(libfuzzy_la_SOURCES) > $(datadir)/fuzzy.def)


libfuzzy_la_LDFLAGS = -release $(LT_RELEASE) $(CYGWIN_LDFLAGS) $(top_srcdir)/../data-flow/src/libflow.la

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/../data-flow/include

LDADD = $(top_srcdir)/src/libfuzzy.la \
        $(top_srcdir)/../data-flow/src/libflow.la
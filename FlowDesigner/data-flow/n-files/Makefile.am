#AUTOMAKE_OPTIONS = no-dependencies

override includedir=${prefix}/include/overflow
override datadir=${prefix}/share/overflow
override libdir=${prefix}/share/overflow/lib

toolboxdir = $(datadir)/toolbox/data-flow

toolbox_DATA = LoadFile.n SaveAs.n SerializeAs.n Print.n ToInt.n ToFloat.n ToBool.n ToDouble.n ToString.n

EXTRA_DIST = $(toolbox_DATA)

#AUTOMAKE_OPTIONS = no-dependencies

override includedir=${prefix}/include/flowdesigner
override datadir=${prefix}/share/flowdesigner
override libdir=${prefix}/share/flowdesigner/lib

toolboxdir = $(datadir)/toolbox/data-flow

toolbox_DATA = LoadFile.n \
	       SaveAs.n \
	       SerializeAs.n \
	       Print.n \
	       ToInt.n \
	       ToFloat.n \
	       ToBool.n \
	       ToDouble.n \
	       ToString.n \
	       Modulo.n \
	       Step.n \
	       Trigger.n \
	       NilObject.n

EXTRA_DIST = $(toolbox_DATA)

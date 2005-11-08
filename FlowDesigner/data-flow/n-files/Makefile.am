#AUTOMAKE_OPTIONS = no-dependencies

override includedir=${prefix}/include/flowdesigner
override datadir=${prefix}/lib/flowdesigner/toolbox/data-flow
override libdir=${prefix}/lib/flowdesigner/toolbox/data-flow
override bindir=${prefix}/lib/flowdesigner/toolbox/data-flow

toolboxdir = $(datadir)

toolbox_DATA = LoadFile.n \
	       SaveAs.n \
	       SerializeAs.n \
	       Print.n \
	       ToInt.n \
	       ToFloat.n \
	       ToBool.n \
	       ToDouble.n \
	       ToString.n \
	       ToVect.n \
	       Modulo.n \
	       Step.n \
	       Trigger.n \
	       NilObject.n

EXTRA_DIST = $(toolbox_DATA)

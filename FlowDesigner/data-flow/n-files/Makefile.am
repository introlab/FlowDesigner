#AUTOMAKE_OPTIONS = no-dependencies

override includedir=${prefix}/include/flowdesigner
override datadir=${prefix}/share/flowdesigner
override libdir=${prefix}/lib

toolboxdir = $(libdir)/toolbox/data-flow/n-files

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

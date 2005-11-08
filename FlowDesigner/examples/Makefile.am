#AUTOMAKE_OPTIONS = no-dependencies
override includedir=${prefix}/share/flowdesigner/examples/include
override datadir=${prefix}/share/flowdesigner/examples
override libdir=${prefix}/share/flowdesigner/examples/lib
override bindir=${prefix}/share/flowdesigner/examples/bin

examplesdir=${prefix}/share/flowdesigner/examples

examples_DATA = demo_if.n  find_beat.n  hello.n  play_mp3.n  tone.n \
	demo_feedback.n jm.wav helium.n reverb.n mixedthread.n dtmf.n

EXTRA_DIST = $(examples_DATA)

#AUTOMAKE_OPTIONS = no-dependencies

override includedir=${prefix}/include/flowdesigner
override datadir=${prefix}/share/flowdesigner
override libdir=${prefix}/share/flowdesigner/lib
examplesdir=${prefix}/share/flowdesigner/examples

examples_DATA = demo_if.n  find_beat.n  hello.n  play_mp3.n  tone.n \
	demo_feedback.n jm.wav helium.n reverb.n mixedthread.n dtmf.n

EXTRA_DIST = $(examples_DATA)

#leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<


ERL = erl -boot start_clean

#List of erlang modules to compile

MODS = add dataFlow constant

all: compile

compile: ${MODS:%=%.beam}



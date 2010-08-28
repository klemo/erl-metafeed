.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = metafeed interpreter feed_parser utils \
	feed_parser_tests utils_tests test_runner

all: compile
	${ERL} -pa $(CURDIR) -s metafeed start

compile: ${MODS:%=%.beam}

test:
	${ERL} -pa $(CURDIR) -s test_runner test -s init stop

clean:
	rm -rf *.beam erl_crash.dump

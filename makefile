ifdef debug
	EFLAGS+=-Ddebug +export_all
endif

.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $(EFLAGS) $<

ERL = erl -boot start_clean

MODS = mf interpreter feed_parser aggregator utils www_feed \
	feed_parser_tests utils_tests mf_tests test_runner

all: compile
	${ERL} -pa $(CURDIR) -s mf start

www: compile
	yaws -i --runmod mf

compile: ${MODS:%=%.beam}

test: compile
	${ERL} -pa $(CURDIR) -s test_runner test -s init stop

clean:
	rm -rf *.beam erl_crash.dump

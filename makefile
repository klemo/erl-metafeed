ifdef debug
	EFLAGS+=-Ddebug +export_all
endif

.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $(EFLAGS) $<

ERL = erl -boot start_clean

ERL_SRC := $(wildcard *.erl)
ERL_OBJ := $(patsubst %.erl,%.beam,${ERL_SRC})

all: compile
	${ERL} -pa $(CURDIR) -s mf start

www: compile
	yaws -i --runmod mf

compile: ${ERL_OBJ}

test: compile
	${ERL} -pa $(CURDIR) -s test_runner test -s init stop

clean:
	rm -rf *.beam erl_crash.dump

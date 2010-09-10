ifdef debug
	EFLAGS+=-Ddebug +export_all
endif

.SUFFIXES: .erl .beam

ERL = erl -boot start_clean

ERL_SRC := $(wildcard src/*.erl)
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})

all: compile
	${ERL} -pa $(CURDIR)/ebin -s mf start

www: compile
	yaws -i --pa $(CURDIR)/ebin --runmod mf

ebin/:
	mkdir ebin

compile: ebin/ ${ERL_OBJ} 

ebin/%.beam: src/%.erl
	erlc -o $(dir $@) -W $(EFLAGS) $<

test: compile
	${ERL} -pa $(CURDIR)/ebin -s test_runner test -s init stop

clean:
	rm -rf ebin/ erl_crash.dump *.log *.access

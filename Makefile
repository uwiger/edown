REBAR3=$(shell which rebar3 || echo ./rebar3)

.PHONY: all compile clean doc

all: compile

compile:
	$(REBAR3) compile

clean:
	$(REBAR3) clean

doc: compile
	./make_doc

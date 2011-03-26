.PHONY: all compile clean doc

all: compile doc

compile:
	./rebar compile

clean:
	./rebar clean

doc: compile
	./make_doc

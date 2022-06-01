ERL       ?= erl
ERLC      ?= $(ERL)c
REBAR3     := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3

.PHONY: all compile clean doc

all: compile

compile:
	$(REBAR3) compile

clean:
	$(REBAR3) clean

doc: compile
	./make_doc

$(REBAR):
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "$(REBAR)"}])' \
	  -s init stop
	chmod +x $(REBAR)
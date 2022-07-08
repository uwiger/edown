SHELL := $(shell which bash)
.DEFAULT_GOAL := compile

export ERL := erl
export REBAR3 := ./rebar3
export REBAR := rebar3
export OS_NAME ?= $(shell uname)
export REBAR_LOCATION_LINUX_IF = $(shell which rebar3 | head -1 | grep rebar3)
export REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3

export REBAR_REQUEST ?= $(ERL) -noshell -s inets -s ssl \
		-eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [{ssl, [{verify,verify_none}]}], [{stream, "$(REBAR3)"}])' \
		-s init stop && chmod +x $(REBAR3) && echo $(REBAR3)
	
# Check if rebar3 exists in current folder
# If yes - use it
# If no - check OS name and if it is Linux -
#  use `which` to get `rebar3` builder from current system.
# Else `rebar3` is downloaded by URL.
### -->>
BUILDER := $(shell [ -f $(REBAR3) ] && echo $(REBAR3) && exit || \
					[ $(OS_NAME) == Linux ] && [ -n $(REBAR_LOCATION_LINUX_IF) ] && which rebar3 && exit || \
					[ -z $(REBAR_LOCATION_LINUX_IF) ] && $(REBAR_REQUEST))		
### <<--	

.PHONY: compile clean doc

compile: ; $(BUILDER) compile

clean: ; $(BUILDER) clean

doc: compile
	./make_doc
	
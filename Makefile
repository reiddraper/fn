.PHONY: test

all: compile

compile:
	@./rebar compile

clean:
	@./rebar clean

test: compile
	@./rebar skip_deps=true eunit

##
## Doc targets
##

docs:
	./rebar skip_deps=true doc

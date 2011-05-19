REBAR := ./rebar

.PHONY: all deps doc test clean update release

all: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

doc:
	$(REBAR) doc skip_deps=true

test:
	$(REBAR) eunit skip_deps=true

clean:
	$(REBAR) clean

update:
	git pull
	$(REBAR) update-deps
	$(REBAR) compile

release: all test
	dialyzer --src src/*.erl

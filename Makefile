all: test dialyzer

REBAR3 ?= rebar3

test:
	$(REBAR3) eunit

dialyzer:
	$(REBAR3) dialyzer

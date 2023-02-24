all: test dialyzer

test:
	$(REBAR3) eunit

dialyzer:
	$(REBAR3) dialyzer

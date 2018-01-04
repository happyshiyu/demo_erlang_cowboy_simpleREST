REBAR3 ?= rebar3

all:
	$(REBAR3) as prod release

debug:
	$(REBAR3) shell

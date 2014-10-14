.PHONY: deps test

REBAR := ./rebar

all:
	@$(REBAR) update-deps
	@$(REBAR) get-deps
	@$(REBAR) compile

compile:
	@$(REBAR) compile skip_deps=true

clean:
	@$(REBAR) clean skip_deps=true
distclean:
	@$(REBAR) delete-deps
	@$(REBAR) clean

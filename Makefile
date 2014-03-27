REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

test:
	@$(REBAR) skip_deps=true ct

run:
	ERL_LIBS=apps:deps erl +K true -name graph@127.0.0.1 -boot start_sasl -s graph_app -sasl errlog_type error

REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

test:
	@TZ="Europe/Moscow" $(REBAR) skip_deps=true ct

run:
	ERL_LIBS=apps:deps erl +K true -name graph@127.0.0.1 -config app -boot start_sasl -s graph_app -sasl errlog_type error

rund:
	ERL_LIBS=apps:deps erl +K true -name graph@127.0.0.1 -config app -boot start_sasl -s graph_app -sasl errlog_type error -detached
	
stop:
	erl -name graph_stop@127.0.0.1 -eval "rpc:call('graph@127.0.0.1', init, stop, [])" -detached -s init stop

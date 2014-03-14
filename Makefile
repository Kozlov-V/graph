compile:
	./rebar compile

run:
	ERL_LIBS=apps:deps erl +K true -name graph@127.0.0.1 -boot start_sasl -s graph_app -sasl errlog_type error

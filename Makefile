
PROJECT = eproxy_client
DEPS = websocket_client
dep_websocket_client = git https://github.com/jeremyong/websocket_client master

#ERLC_OPTS += -Ddebug
#ERLC_OPTS += +debug_info
ERLC_OPTS += +no_debug_info

ESCRIPT_NAME = epc
ESCRIPT_EMU_ARGS = +K true -config client -detached -escript main eproxy_client

include erlang.mk

package :
	make escript
	mv epc priv/epc
	zip -j epc.zip priv/*
	rm priv/epc

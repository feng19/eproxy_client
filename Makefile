PROJECT = eproxy_client
DEPS = websocket_client
websocket_client = git https://github.com/jeremyong/websocket_client v0.7

#ERLC_OPTS += +debug_info
ERLC_OPTS += -Ddebug
ERLC_OPTS += +no_debug_info

include erlang.mk

PROJECT = eproxy_client
BUILD_DEPS = reload_mk
DEP_PLUGINS = reload_mk
DEPS = websocket_client
dep_websocket_client = git https://github.com/jeremyong/websocket_client v0.7

#ERLC_OPTS += -Ddebug
#ERLC_OPTS += +debug_info
ERLC_OPTS += +no_debug_info

include erlang.mk

#rebar:: rebar.config

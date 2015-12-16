#! /bin/bash

SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`

cd "$WORKDIR"


running=`ps -ef | grep eproxy_client | wc -l`
if [[ running -ge 2 ]]
then
    echo "alreay running"
    exit -1
else
    #erl -pa ebin deps/*/ebin -config client +K true -s eproxy_client start -detached
    erl -pa ebin deps/*/ebin -config client +K true -s eproxy_client start 
fi

exit 0


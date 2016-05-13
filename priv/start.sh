#! /bin/bash

SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`

cd "$WORKDIR"


running=`ps -ef | grep epc | wc -l`
if [[ running -ge 2 ]]
then
    echo "alreay running"
    exit -1
else
    escript epc
    echo "eproxy client startd!"
fi

exit 0


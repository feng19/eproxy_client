eproxy_client(epc)
==================

## Introduction ##

* this app use for cross the GFW
* and this is just a client
* so you must deploy the server([eproxy_server](https://github.com/feng19/eproxy_server)) first

## Feature ##

* support http\socks4\socks5 protocol
* protocol encrypt

## Usage ##
* make & setting
```
    git clone git://github.com/feng19/eproxy_client
    cd eproxy_client
    make
    cp client.config.sample client.config
    vi client.config(setting server address & local port)
    ./start_client.sh or ./start_client.sh -d
```
* setting browser plugin
* just use


-define(DEBUG(Msg),eproxy_client:debug(?MODULE,?LINE,Msg)).
-define(DEBUG(Format,Args),eproxy_client:debug(?MODULE,?LINE,Format,Args)).

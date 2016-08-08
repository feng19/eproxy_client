
-include("socks_type.hrl").
-include("debug.hrl").
-define(TIMEOUT, 1000 * 20).

%%1-no encrypt packet 2-just encrypt first packet 3-encrypt all packet
-define(NOT_ENCRYPT,    1).
-define(FIRST_ENCRYPT,  2).
-define(ALL_ENCRYPT,    3).
-module(mqtt_test).
-include_lib("eunit/include/eunit.hrl").
-include("src/mqtt.hrl").

extract_header_test_() ->
    [
        ?_assert(mqtt:extract_header(<<1:4, 0:1, 1:2, 0:1, "extra">>) =:= {#mqtt_header{type=1, dup=0, qos=1, retain=0}, <<"extra">>})
    ].
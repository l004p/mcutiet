-module(mqtt_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/mqtt.hrl").

extract_header_test_() ->
    [
        ?_assert(mqtt:extract_header(<<1:4, 0:1, 1:2, 0:1, "extra">>) =:= {#mqtt_header{type=1, dup=0, qos=1, retain=0}, <<"extra">>})
    ].

packet_match_test_() ->
    [
        ?_assert(mqtt:packet_match({#mqtt_header{type=1, dup=0, qos=0, retain=0}, <<"extra">>}) =:= <<"extra">>),
        ?_assert(mqtt:packet_match({#mqtt_header{type=1, dup=2, qos=0, retain=0}, <<"extra">>}) =:= false),
        ?_assert(mqtt:packet_match({#mqtt_header{type=3, dup=2, qos=0, retain=1}, <<"extra">>}) =:= true),
        ?_assert(mqtt:packet_match({#mqtt_header{type=3, dup=2, qos=3, retain=0}, <<"extra">>}) =:= true)
    ].
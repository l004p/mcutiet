%% mqtt protocol

-module(mqtt).
-export([extract_header/1, packet_match/1]).
-include("mqtt.hrl").

extract_header(Raw) ->
    <<
        Type:4,
        Dup:1,
        Qos:2,
        Retain:1,
        Tail/binary
    >> = Raw,
    {#mqtt_header{
        type=Type,
        dup=Dup,
        qos=Qos,
        retain=Retain
        }, Tail}.

packet_match({?CONNECT_HEADER, Tail}) -> parse_connect(Tail);
packet_match({?PUBLISH_HEADER, _}) -> true; %% just checking pattern matching extent for records -> only compares the type part of the record
packet_match({_,_}) -> false.
    

parse_connect(Tail) -> Tail.

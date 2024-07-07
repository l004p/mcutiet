%% mqtt protocol

-module(mqtt).
-export([extract_header/1, packet_match/2, extract_length/1, decode_packet/1]).
-include("mqtt.hrl").

extract_header(<<
        Type:4,
        Dup:1,
        Qos:2,
        Retain:1,
        Tail/binary
    >>) ->
    {#mqtt_header{
        type=Type,
        dup=Dup,
        qos=Qos,
        retain=Retain
        }, Tail}.

decode_packet(Raw) -> 
    {Header, Rest} = extract_header(Raw),
    {Length, Data} = extract_length(Rest),
    Packet = extract_packet(Header, Length, Data),
    Packet.
    

extract_length(<<0:1, L:7, Data/binary>>) ->
    {L, Data};
extract_length(<<1:1, L1:7, 0:1, L2:7, Data/binary>>) ->
    {L1 + (L2 bsl 7), Data};
extract_length(<<1:1, L1:7, 1:1, L2:7, 0:1, L3:7, Data/binary>>) ->
    {L1 + (L2 bsl 7) + (L3 bsl 14), Data};
extract_length(<<1:1, L1:7, 1:1, L2:7, 1:1, L3:7, 0:1, L4:7, Data/binary>>) ->
    {L1 + (L2 bsl 7) + (L3 bsl 14) + (L4 bsl 21), Data};
extract_length(_) ->
    {error, malformed_length}.


packet_match(?CONNECT_HEADER, Tail) -> true;
packet_match(?PUBLISH_HEADER, _) -> true; %% just checking pattern matching extent for records -> only compares the type part of the record
packet_match(_,_) -> false.
    
extract_packet(Header, Length, Data) -> true.

parse_connect(Data) -> 
    <<
        0:8,
        4:8,
        "MQTT",
        5:8,
        Username_Flag:1,
        Password_Flag:1,
        Will_Retain:1,
        Will_Qos:2,
        Will_Flag:1,
        Clean_Start:1,
        0:1, %% reserved
        Keepalive_MSB:8,
        Keepalive_LSB:8,
        Rest/binary
    >> = Data,
    {Properties_Length, Rest} = extract_length(Rest),
    <<Properties:Properties_Length/binary, Payload/binary>> = Rest,

    #mqtt_connect{}.

%% mqtt protocol

-module(mqtt).
-export([extract_header/1]).
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



%% connack packet

%% publish packet

%% pubrec packet

%% puback packet

%% pubrel packet

%% pubcomp packet

%% subscribe packet

%% suback packet

%% unsubscribe packet

%% unsuback packet

%% pingreq packet

%% pingresp packet

%% disconnect packet

%% auth packet





-ifndef(MQTT_HRL).
-define(MQTT_HRL, true).


-define(MQTT_HEADER_LEN, 2).
-define(MQTT_ACK_LEN, 4).



-define(RESERVED, 0).

%% packet type
-define(CONNECT, 1).
-define(CONNACK, 2).
-define(PUBLISH, 3).
-define(PUBREC, 4).
-define(PUBRACK, 5).
-define(PUBREL, 6).
-define(PUBCOMP, 7).
-define(SUBSCRIBE, 8).
-define(SUBACK, 9).
-define(UNSUBSCRIBE, 10).
-define(UNSUBACK, 11).
-define(PINGREQ, 12).
-define(PINGRESP, 13).
-define(DISCONNECT, 14).
-define(AUTH, 15). %% mqtt 5 only uwu

%% QoS level
-define(QOS_0, 0).
-define(QOS_1, 1).
-define(QOS_2, 2).

%% header
-record(mqtt_header, {
    type, %% 4
    dup, %% 1,
    qos, %% 2,
    retain %% 1 
}).

-define(CONNECT_HEADER, 
    #mqtt_header{
        type=1,
        dup=0,
        qos=0,
        retain=0
}).

-define(CONNACK_HEADER, 
    #mqtt_header{
        type=2,
        dup=0,
        qos=0,
        retain=0
}).

-define(PUBLISH_HEADER, 
    #mqtt_header{
        type=3 %% other bits are variable
}).

-define(PUBACK_HEADER, 
    #mqtt_header{
        type=4,
        dup=0,
        qos=0,
        retain=0
}).

-define(PUBREC_HEADER, 
    #mqtt_header{
        type=5,
        dup=0,
        qos=0,
        retain=0
}).

-define(PUBREL_HEADER, 
    #mqtt_header{
        type=6,
        dup=0,
        qos=1,
        retain=0
}).

-define(PUBCOMP_HEADER, 
    #mqtt_header{
        type=7,
        dup=0,
        qos=0,
        retain=0
}).

-define(SUBSCRIBE_HEADER, 
    #mqtt_header{
        type=8,
        dup=0,
        qos=1,
        retain=0
}).

-define(SUBACK_HEADER, 
    #mqtt_header{
        type=9,
        dup=0,
        qos=0,
        retain=0
}).

-define(UNSUBSCRIBE_HEADER, 
    #mqtt_header{
        type=10,
        dup=0,
        qos=1,
        retain=0
}).

-define(UNSUBACK_HEADER, 
    #mqtt_header{
        type=11,
        dup=0,
        qos=0,
        retain=0
}).

-define(PINGREQ_HEADER, 
    #mqtt_header{
        type=12,
        dup=0,
        qos=0,
        retain=0
}).

-define(PINGRESP_HEADER, 
    #mqtt_header{
        type=13,
        dup=0,
        qos=0,
        retain=0
}).

-define(DISCONNECT_HEADER, 
    #mqtt_header{
        type=14,
        dup=0,
        qos=0,
        retain=0
}).

-define(AUTH_HEADER, 
    #mqtt_header{
        type=15,
        dup=0,
        qos=0,
        retain=0
}).

%% connect packet
-record(mqtt_connect , {
    %% fixed header - 1 byte - connect is always 0x10
    header :: #mqtt_header{},
    length_msb,
    length_lsb,
    %% variable header
    %% protocol name - X bytes
    name = <<"MQTT">>,
    %% protocol version - 1 byte
    version = 5,
    %% connect flags - 1 byte
    reserved = 0,
    clean_start,
    will_flag,
    will_qos,
    will_retain,
    password_flag,
    username_flag,
    %% double byte max time between 2 control packets
    keepalive_msb,
    keepalive_lsb,
    %% properties - X bytes
        %% 0x11 - session expiry interval - 4 byte integer
        %% 0x21 - receive maximum - 2 byte integer
        %% 0x27 - maximum packet size - 4 byte integer
        %% 0x22 - topic alias maximum - 2 byte integer
        %% 0x19 - request response info - byte
        %% 0x17 - request problem info - byte
        %% 0x26 - user property - UTF-8 string pair
        %% 0x15 - auth method - UTF-8 encoded string
        %% 0x16 - auth data - binary data
    properties,
    %% payload - all optional except client id, in order of connect flags
        %% client id - mandatory
        %% will properties
        %% will topic
        %% will payload
        %% user name
        %% password
    client_id, 
    will_props,
    will_topic,
    will_payload,
    username,
    password
}).


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


-endif.
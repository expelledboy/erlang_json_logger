-module(erlang_json_logger).

%% logger callbacks
-export([ format/2 ]).

-ifdef(TEST).
-export([ format_msg/3 ]).
-export([ pre_encode/2 ]).
-endif.

%% -- logger callbacks

-spec format(LogEvent, Config) -> unicode:chardata() when
      LogEvent :: logger:log_event(),
      Config :: logger:formatter_config().

format(#{level:=Level, msg:={report, Msg}, meta:=Meta}, Config) when is_map(Msg) ->
    Meta0 = maps:put(level, Level, Meta),
    format_msg(Msg, Meta0, Config);
format(Map = #{msg := {report, KeyVal}}, Config) when is_list(KeyVal) ->
    format(Map#{msg := {report, maps:from_list(KeyVal)}}, Config);
format(Map = #{msg := {string, String}}, Config) ->
    Msg = #{text => unicode:characters_to_binary(String)},
    format(Map#{msg := {report, Msg}}, Config);
format(Map = #{msg := {Format, Terms}}, Config) ->
    String = unicode:characters_to_binary(io_lib:format(Format, Terms)),
    Msg = #{text => String},
    format(Map#{msg := {report, Msg}}, Config).

%% -- private

format_msg(Msg, Meta, Config) when
      is_map(Msg) andalso
      is_map(Meta) andalso
      is_list(Config) ->
    Meta0 = maps:with([domain, file, label, level, line, logger_formatter, mfa, pid, report, time], Meta),
    Msg0 = maps:merge(Msg, Meta0),
    % io:format("Msg0: ~p~n", [Msg0]),
    Msg1 = map_nested(fun pre_encode/2, Msg0),
    encode(Msg1, Config).

%% --
map_nested(Fun, Map) ->
    map_nested(Fun, root, Map).

map_nested(Fun, _Key, Map) when is_map(Map) ->
    maps:from_list([
        {Key, map_nested(Fun, Key, Value)} ||
        {Key, Value} <- maps:to_list(Map)
    ]);
map_nested(Fun, Key, Value) ->
    Fun(Key,Value).

%% --

encode(Map, Config) ->
    try
        JSON = jsx:encode(Map, Config),
        <<JSON/binary, "\n">>
    catch
        error:Error ->
            io:format("jsx ~p ~p: ~p~n", [Error, Map, maps:to_list(Config)]),
            throw(Error)
    end.


pre_encode(file,Value) when is_list(Value) ->
    list_to_binary(filename:basename(Value));
pre_encode(time,SysTime) when is_integer(SysTime) ->
    StrValue = calendar:system_time_to_rfc3339(SysTime div 1000,[{unit,millisecond}]),
    list_to_binary(StrValue);

pre_encode(_Key,T) when is_atom(T) ->
    atom_to_binary(T, utf8);
pre_encode(_Key,PidRef) when is_pid(PidRef) or is_reference(PidRef) ->
    list_to_binary(pid_to_list(PidRef));
pre_encode(_Key,Fun) when is_function(Fun) ->
    <<"#function">>;
pre_encode(_Key,Tuple) when is_tuple(Tuple) ->
    list_to_binary(io_lib:format("~tp", [Tuple]));
pre_encode(_Key,Map) when is_map(Map) ->
    maps:from_list([
        {Key, pre_encode(Key,Value)} || {Key, Value} <- maps:to_list(Map)
    ]);
pre_encode(Key,List) when is_list(List) ->
    case io_lib:printable_list(List) of
        true ->
            case unicode:characters_to_binary(List) of
                {_, _, _} -> % error or incomplete
                    list_to_binary(io_lib:format("~tp", [List]));
                Binary ->
                    Binary
            end;
        false ->
            case is_proplist(List) of
                true ->
                    pre_encode(Key,maps:from_list(List));
                false ->
                    list_to_binary(io_lib:format("~tp", [List]))
            end
    end;
pre_encode(Key,[{Pid1, Pid2} | Rest]) when is_pid(Pid1), is_pid(Pid2) ->
    [{pre_encode(Key,Pid1), pre_encode(Key,Pid2)} | pre_encode(Key,Rest)];
pre_encode(Key,[{Pid, Other} | Rest]) when is_pid(Pid) ->
    [{pre_encode(Key,Pid),Other} | pre_encode(Key,Rest)];
pre_encode(_Key,Other) ->
    Other.

%% --

is_proplist([]) -> true;
is_proplist([{K,_}|L]) when is_atom(K) -> is_proplist(L);
is_proplist(_) -> false.

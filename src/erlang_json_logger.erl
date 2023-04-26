-module(erlang_json_logger).

%% logger callbacks
-export([ format/2 ]).

-ifdef(TEST).
-export([ format_msg/3 ]).
-export([ pre_encode/1 ]).
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
    Msg1 = map_nested(fun pre_encode/1, Msg0),
    encode(Msg1, Config).

%% --

map_nested(Fun, Map) when is_map(Map) ->
    maps:from_list([
        {Key, map_nested(Fun, Value)} ||
        {Key, Value} <- maps:to_list(Map)
    ]);
map_nested(Fun, Value) ->
    Fun(Value).

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

pre_encode(T) when is_atom(T) ->
    atom_to_binary(T, utf8);
pre_encode(PidRef) when is_pid(PidRef) or is_reference(PidRef) ->
    list_to_binary(pid_to_list(PidRef));
pre_encode(Fun) when is_function(Fun) ->
    <<"#function">>;
pre_encode(Tuple) when is_tuple(Tuple) ->
    list_to_binary(io_lib:format("~tp", [Tuple]));
pre_encode(Map) when is_map(Map) ->
    maps:from_list([
        {Key, pre_encode(Value)} || {Key, Value} <- maps:to_list(Map)
    ]);
pre_encode(List) when is_list(List) ->
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
                    pre_encode(maps:from_list(List));
                false ->
                    list_to_binary(io_lib:format("~tp", [List]))
            end
    end;
pre_encode([{Pid1, Pid2} | Rest]) when is_pid(Pid1), is_pid(Pid2) ->
    [{pre_encode(Pid1), pre_encode(Pid2)} | pre_encode(Rest)];
pre_encode([{Pid, Other} | Rest]) when is_pid(Pid) ->
    [{pre_encode(Pid), Other} | pre_encode(Rest)];
pre_encode(Other) ->
    Other.

%% --

is_proplist([]) -> true;
is_proplist([{K,_}|L]) when is_atom(K) -> is_proplist(L);
is_proplist(_) -> false.

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
      is_map(Config) ->
    Defaults = #{ indent => 2 },
    Config0 = maps:merge(Defaults, Config),
    Msg0 = maps:merge(Msg, Meta),
    encode(Msg0, Config0).

%% --

encode(Map, Config) ->
    jsx:encode(maps:to_list(Map),
               maps:to_list(Config)++[{pre_encode, fun pre_encode/1}]).

pre_encode(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
pre_encode([{Pid1, Pid2} | Rest]) when is_pid(Pid1), is_pid(Pid2) ->
    [{pre_encode(Pid1), pre_encode(Pid2)} | pre_encode(Rest)];
pre_encode([{Pid, Other} | Rest]) when is_pid(Pid) ->
    [{pre_encode(Pid), Other} | pre_encode(Rest)];
pre_encode(Other) ->
    Other.

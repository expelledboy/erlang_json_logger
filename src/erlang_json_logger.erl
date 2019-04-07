-module(erlang_json_logger).

%% logger callbacks
-export([ format/2 ]).

-ifdef(TEST).
-export([ format_msg/3 ]).
-endif.

%% -- logger callbacks

-spec format(LogEvent, Config) -> unicode:chardata() when
      LogEvent :: logger:log_event(),
      Config :: logger:formatter_config().

format(#{level:=Level, msg:={report, Msg}, meta:=Meta}, Config) when is_map(Msg) ->
    Config0 = defaults(config, Config),
    Meta0 = maps:put(level, Level, Meta),
    format_msg(Msg, Meta0, Config0);
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

defaults(config, Map) ->
    Defaults = #{ indent => 2 },
    maps:merge(Defaults, Map).

format_msg(Msg, Meta, Config) ->
    Details = maps:merge(Msg, Meta),
    jsx:encode(Details, maps:to_list(Config)).

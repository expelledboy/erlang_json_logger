-module(log_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MOD, erlang_json_logger).

all() ->
    [
     basic_test,
     pid_test
    ].

basic_test(_) ->
    Log = #{level => info,
            msg => {report, #{what => crash}},
            meta => #{}},
    ?assertEqual(
       <<"{\n  \"level\":\"info\",\n  \"what\":\"crash\"\n}">>,
       ?MOD:format(Log, #{})
      ).

pid_test(_) ->
    Log = #{level => info,
            msg => {report, #{pid => self()}},
            meta => #{}},
    PidBin = list_to_binary(pid_to_list(self())),
    ?assertEqual(
       <<"{\n  \"level\":\"info\",\n  \"pid\":\"", PidBin/binary, "\"\n}">>,
       ?MOD:format(Log, #{})
      ).

-module(log_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MOD, erlang_json_logger).

all() ->
    [basic_test].

basic_test(_) ->
    Log = #{level => info,
            msg => {report, #{what => crash}},
            meta => #{}},
    ?assertEqual(
       <<"{\n  \"level\":\"info\",\n  \"what\":\"crash\"\n}">>,
       ?MOD:format(Log, #{})
      ).

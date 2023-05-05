-module(log_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(MOD, erlang_json_logger).

all() ->
    [
        basic_test,
        pid_test,
        file_test,
        time_test
    ].

basic_test(_) ->
    Log = #{
        level => info,
        msg => {report, #{what => crash}},
        meta => #{}
    },
    ?assertEqual(
        <<"{\"level\":\"info\",\"what\":\"crash\"}\n">>,
        ?MOD:format(Log, [])
    ).

pid_test(_) ->
    Log = #{
        level => info,
        msg => {report, #{pid => list_to_pid("<0.228.0>")}},
        meta => #{}
    },
    ?assertEqual(
        <<"{\"level\":\"info\",\"pid\":\"<0.228.0>\"}\n">>,
        ?MOD:format(Log, [])
    ).

file_test(_) ->
    Log = #{
        level => info,
        msg => {report, #{file => "/long/path/test.erl"}},
        meta => #{}
    },
    ?assertEqual(
        <<"{\"file\":\"test.erl\",\"level\":\"info\"}\n">>,
        ?MOD:format(Log, [])
    ).

time_test(_) ->
    Log = #{
        level => info,
        msg => {report, #{}},
        meta => #{time => 1683269043433248}
    },
    ?assertEqual(
        <<"{\"level\":\"info\",\"time\":\"2023-05-05T08:44:03.433+02:00\"}\n">>,
        ?MOD:format(Log, [])
    ).

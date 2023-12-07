Erlang JSON Logger
=====

> A custom erlang logger formatter which prints JSON

Typically Log Shippers like Filebeat, Fluentd, Logstash, etc. expect logs to be
in JSON format. This is a custom logger formatter for Erlang that prints JSON.

Usage
-----

Add dependency to your `rebar.config` file;

```erlang
{deps, [erlang_json_logger]}.
```

Configure the logger in your `sys.config` file;

```erlang
[
 {kernel, [
    {logger, [
        {handler, default, logger_std_h,
         #{formatter => {erlang_json_logger, #{
            indent => 2
          }}}
        }
    ]},
    {logger_level, info}
 ]}
].
```

Then, use the logger in your code;

Either using the `?LOG_*` macros;

```erlang
-include_lib("kernel/include/logger.hrl").

login(Username, Password) ->
   case db:login(Username, Password) of
      ok ->
         ?LOG_INFO("user ~s logged in", [Username]);
      {error, Reason} ->
         ?LOG_ERROR("user ~s failed to login: ~p", [Username, Reason])
   end.
```

```erlang
{"file":"user_auth.erl","level":"info","line":18,"mfa":"{user_auth,login,2}","pid":"<0.164.0>","text":"user joe logged in","time":"2023-12-07T20:24:42.315+02:00"}
```

Or using the `logger` module;

```erlang
1> logger:info("simple message").
{"level":"info","pid":"<0.144.0>","text":"simple message","time":"2023-12-07T20:15:46.107+02:00"}
ok
2> logger:info("Hello, ~s!~n", ["World"]).
{"level":"info","pid":"<0.144.0>","text":"Hello, World!\n","time":"2023-12-07T20:15:51.059+02:00"}
ok
3> logger:info(#{ action => domain_event, info => [1,2,3], more_info => meta, result => "Success!" }).
{"action":"domain_event","info":"[1,2,3]","level":"info","more_info":"meta","pid":"<0.144.0>","result":"Success!","time":"2023-12-07T20:16:34.443+02:00"}
ok
```

Happy logging!

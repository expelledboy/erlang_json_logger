Erlang JSON Logger
=====

A custom erlang logger formatter which prints JSON

Usage
-----

`rebar.config`;

```erlang
{deps, [erlang_json_logger]}.
```

`sys.config`;

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

in source code;

```erlang
?LOG_ERROR(
    "user password not strong enough",
    #{ action => change_password, result => {error, entropy_too_low} }
).
```

**NOTE** when building a release add as dependency manually.

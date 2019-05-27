# parse_transform_compiled_regex

Provides a simple cooldown filter to use in otp's logger.  
This filter stops events from reaching the handler if the event's
key has already triggered a cooldown timer.
You can provide custom functions to generate the key and cooldown
for an event.

## How to use:

Include it as a dependency and add the filter to any handler (`sys.config`
example):
```
[{kernel,
  [{logger,
    [{handler, default, logger_std_h, #{filters => [{cooldown, {fun logger_cooldown_filter:try_log_cooldown/2, undefined}}]}}
    ]}]}
].
```
(Dont forget to start the application!)

## Run tests:
```
rebar3 ct
```

## Authors

* **Jose M Perez Ramos** - [Kuroneer](https://github.com/Kuroneer)

## License

[MIT License](LICENSE)


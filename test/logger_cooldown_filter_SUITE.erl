%%%-------------------------------------------------------------------
%%% Part of logger_cooldown_filter Erlang App
%%% MIT License
%%% Copyright (c) 2019 Jose Maria Perez Ramos
%%%-------------------------------------------------------------------
-module(logger_cooldown_filter_SUITE).
-compile(export_all).

all() ->
    [independent_cooldowns].

init_per_suite(Config) ->
    application:ensure_all_started(logger_cooldown_filter),
    Config.

end_per_suite(Config) ->
    application:stop(logger_cooldown_filter),
    Config.

init_pert_test_case(_TestCase, Config) ->
    ets:delete_all_objects(logger_cooldown_filter),
    Config.


%%====================================================================
%% Test cases
%%====================================================================

independent_cooldowns(_Config) ->
    LogEvent = #{
      level => error,
      msg => {"Tis but a scratch!", []}
     },

    LogEvent = logger_cooldown_filter:try_log_cooldown(LogEvent, undefined),
    stop = logger_cooldown_filter:try_log_cooldown(LogEvent, undefined),

    OtherLogEvent = LogEvent#{msg => {"Message with params: ~p", ['param']}},
    OtherLogEvent = logger_cooldown_filter:try_log_cooldown(OtherLogEvent, undefined),
    stop = logger_cooldown_filter:try_log_cooldown(OtherLogEvent, undefined),
    % This event gets other key
    OtherLogEventWithOtherParam = LogEvent#{msg => {"Message with params: ~p", ['other']}},
    true = OtherLogEventWithOtherParam /= OtherLogEvent,
    OtherLogEventWithOtherParam = logger_cooldown_filter:try_log_cooldown(OtherLogEventWithOtherParam, undefined),

    % The key includes the level
    WarningLogEvent = LogEvent#{level => warning},
    WarningLogEvent = logger_cooldown_filter:try_log_cooldown(WarningLogEvent, undefined),
    stop = logger_cooldown_filter:try_log_cooldown(WarningLogEvent, undefined),

    % alerts are never throttled
    AlertLogEvent = LogEvent#{level => alert},
    AlertLogEvent = logger_cooldown_filter:try_log_cooldown(AlertLogEvent, undefined),
    AlertLogEvent = logger_cooldown_filter:try_log_cooldown(AlertLogEvent, undefined),

    % custom key_generator functions are supported
    Options = [{key_generator_mfa, {?MODULE, key_generator, [1,2,3]}}],
    LogEvent = logger_cooldown_filter:try_log_cooldown(LogEvent, Options),
    stop = logger_cooldown_filter:try_log_cooldown(LogEvent, Options),
    timer:sleep(50),
    LogEvent = logger_cooldown_filter:try_log_cooldown(LogEvent, Options),
    stop = logger_cooldown_filter:try_log_cooldown(LogEvent, Options),
    ok.

key_generator(_LogEvent, _FilterArgs, 1, 2, 3) ->
    {key, 1}.


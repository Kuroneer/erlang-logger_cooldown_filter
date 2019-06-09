%%%-------------------------------------------------------------------
%%% @doc logger_cooldown_filter
%%% Provides a simple cooldown filter to use in otp's logger.
%%% This filter stops events from reaching the handler if the event's
%%% key has already triggered a cooldown timer.
%%%
%%% You can provide custom functions to generate the key and cooldown
%%% for an event.
%%%
%%% By default, it only filters same non-report messages below critical
%%% level.
%%%
%%% @end
%%% Part of logger_cooldown_filter Erlang App
%%% MIT License
%%% Copyright (c) 2019 Jose Maria Perez Ramos
%%%-------------------------------------------------------------------
-module(logger_cooldown_filter).

%% API
-export([
         try_log_cooldown/2,
         get_log_event_key/2
        ]).

%% API called from supervisor
-export([start_link/0]).

-behaviour(gen_server).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).


%%====================================================================
%% Types
%%====================================================================

-type key_and_cooldown() :: undefined | {term(), non_neg_integer()}.
-export_type([key_and_cooldown/0]).


%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec try_log_cooldown(logger:log_event(), logger:filter_arg()) -> logger:filter_return().
try_log_cooldown(LogEvent, FilterArgs) ->
    case get_key(LogEvent, FilterArgs) of
        undefined ->
            LogEvent;
        {undefined, _CooldownMs} ->
            LogEvent;
        {Key, CooldownMs} when is_integer(CooldownMs), CooldownMs > 0 ->
            Expiration = erlang:monotonic_time() + erlang:convert_time_unit(CooldownMs, millisecond, native),
            case catch ets:update_counter(?MODULE, Key, 1, {Key, 0, Expiration}) of
                {'EXIT', _Reason} ->
                    LogEvent;
                1 ->
                    erlang:send_after(CooldownMs, ?MODULE, {clear, Key}),
                    LogEvent;
                _ ->
                    stop
            end;
        _ ->
            LogEvent
    end.

-spec get_log_event_key(logger:log_event(), logger:filter_arg()) -> key_and_cooldown().
get_log_event_key(_LogEvent = #{meta := #{logger_filter_key := undefined}}, _FilterArgs) -> undefined;
get_log_event_key(_LogEvent = #{meta := #{
                                  logger_filter_key := Key,
                                  logger_filter_cooldownms := CooldownMs
                                 }}, _FilterArgs) when is_integer(CooldownMs), CooldownMs > 0 -> {Key, CooldownMs};
get_log_event_key(_LogEvent = #{msg := {report, _}}, _FilterArgs) -> undefined; % Reports bypass the filter
get_log_event_key(_LogEvent = #{level := emergency}, _FilterArgs) -> undefined;
get_log_event_key(_LogEvent = #{level := alert    }, _FilterArgs) -> undefined;
get_log_event_key(_LogEvent = #{level := critical }, _FilterArgs) -> undefined;
get_log_event_key( LogEvent = #{level := Level, msg := Msg}, _FilterArgs) ->
    CooldownMs = case Level of
                     error   ->  1000;
                     warning ->  2000;
                     notice  ->  5000;
                     info    -> 10000;
                     _       -> 30000
                 end,

    Location = case LogEvent of
                   #{meta := Meta} ->
                       {maps:get(line, Meta, undefined), maps:get(file, Meta, undefined)};
                   _ ->
                       undefined
               end,

    Key = case Msg of
              {string, String} -> {Level, Location, String};
              {Format, Args}   ->
                  ArgsWithoutPids = [ Arg || Arg <- Args, not is_pid(Arg) ],
                  {Level, Location, erlang:phash2(Format), erlang:phash2(ArgsWithoutPids)};
              _                -> {Level, Location, erlang:phash2(Msg)}
          end,
    {Key, CooldownMs}.


%%====================================================================
%% gen_server callbacks
%%====================================================================

-define(SWEEP_INTERVAL_MS, 60 * 60 * 1000).

init([]) ->
    ets:new(?MODULE, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    SweepRef = make_ref(),
    erlang:send_after(?SWEEP_INTERVAL_MS, self(), {sweep, SweepRef}),
    {ok, SweepRef}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({sweep, SweepRef}, SweepRef) ->
    ExpiredMS = [{{'_', '_', '$1'}, [{'<', '$1', erlang:monotonic_time()}], [true]}],
    ets:select_delete(?MODULE, ExpiredMS),
    NewSweepRef = make_ref(),
    erlang:send_after(?SWEEP_INTERVAL_MS, self(), {sweep, NewSweepRef}),
    {noreply, NewSweepRef};

handle_info({clear, Key}, State) ->
    ets:delete(?MODULE, Key),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%%====================================================================
%% Internal functions
%%====================================================================

get_key(LogEvent, FilterArgs) when is_list(FilterArgs) ->
    case lists:keyfind(key_generator_mfa, 1, FilterArgs) of
        false -> get_log_event_key(LogEvent, FilterArgs);
        {key_generator_mfa, {M, F, A}} -> apply(M, F, [LogEvent, FilterArgs | A])
    end;

get_key(LogEvent, FilterArgs) ->
    get_log_event_key(LogEvent, FilterArgs).


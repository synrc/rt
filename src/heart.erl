-module(heart).

-include_lib("rt/include/pi.hrl").

-export([server/3]).

server(init, _, #pi{} = Async) ->
    Timer = timer_restart(ping()),
    io:format("INIT "),
    {ok, Async#pi{state = Timer}};
server(ping, _, #pi{state = Timer} = Async) ->
    erlang:cancel_timer(Timer),
    io:format("PING "),
    {ok, Async#pi{state = timer_restart(ping())}};
server(OK, _, #pi{state = Timer} = Async) ->
    erlang:cancel_timer(Timer),
    io:format("OK: ~p~n", [OK]),
    {ok, Async#pi{state = timer_restart(ping())}}.

timer_restart(Diff) ->
    {X, Y, Z} = Diff,
    erlang:send_after(1000 * (Z + 60 * Y + 60 * 60 * X),
                      self(),
                      ping).

ping() -> application:get_env(rt, heart, {0, 1, 0}).

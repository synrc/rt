-module(srv).

-export([start_link/1, spawn_link/1, loop/4]).

-include_lib("rt/include/pi.hrl").

name(App) -> list_to_atom(lists:concat([otp, App])).

emulate_otp(Parent, App) ->
    catch unregister(name(App)),
    catch register(name(App), self()),
    process_flag(trap_exit, true),
    put('$ancestors', [Parent]),
    put('$initial_call',
        {application_controller, start, 1}).

start_link(#pi{} = PI) ->
    X = spawn(srv, spawn_link, [PI]),
    X ! {init, self(), []},
    X.

spawn_link(#pi{parent = Parent, mod = Mod,
               name = Name, hibernate = Hibernate} =
               PI) ->
    emulate_otp(Parent, Name),
    loop(Parent, {local, srv}, PI, Mod, Hibernate).

loop(Parent, Name, State, Mod, hibernate) ->
    erlang:hibernate(?MODULE,
                     ?FUNCTION_NAME,
                     [Parent, Name, State, Mod]);
loop(Parent, Name, State, Mod, Time) ->
    server_loop(loop(Time), Parent, Name, State, Mod).

loop(Parent, Name, State, Mod) ->
    server_loop(loop(), Parent, Name, State, Mod).

loop() -> receive Input -> Input end.

loop(Timeout) ->
    receive
        Input -> Input after Timeout -> {timeout, [], []}
    end.

reply({To, Tag}, Reply) -> To ! {Tag, Reply}.

server_loop({Fun, Sender, Msg}, Parent, Name, State,
            Mod) ->
    try dispatch(call(Fun, Msg, Sender, State, Mod),
                 Sender,
                 Parent,
                 Name,
                 Mod)
    catch
        Error:Reason:Stack ->
            Crash = {Error, Reason, Stack},
            Parent ! {crash, Crash},
            io:format("Exception: ~p~n", [Crash])
    end;
server_loop(Msg, Parent, Name, State, Mod) ->
    server_loop({'$gen_cast', {self(), []}, Msg},
                Parent,
                Name,
                State,
                Mod).

dispatch(Call, Sender, Parent, Name, Mod) ->
    Time = infinity,
    case Call of
        {noreply, State} ->
            loop(Parent, Name, State, Mod, Time);
        {stop, _, _} -> ok;
        {stop, Reply, F, _} ->
            reply(F, Reply),
            Reply;
        {ok, Reply, State} ->
            reply(Sender, Reply),
            loop(Parent, Name, State, Mod, Time);
        {ok, State} ->
            loop(Parent, Name, State, Mod, Time)
    end.

call(Fun, Msg, Sender, State, Mod) ->
    case Fun of
        init -> Mod:server(init, Sender, State);
        timeout -> Mod:server(timeout, Sender, State);
        system -> Mod:server(terminate, Sender, State);
        'EXIT' -> Mod:server(terminate, Sender, State);
        '$gen_call' -> Mod:server(Msg, Sender, State);
        '$gen_cast' -> Mod:server(Msg, Sender, State)
    end.

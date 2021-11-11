-module(srv).

-export([start/1, loop/4, setup_loop/1]).

-include_lib("rt/include/pi.hrl").

name(App) -> list_to_atom(lists:concat([otp, App])).

setup(Parent, App) ->
    catch unregister(name(App)),
    catch register(name(App), self()),
    process_flag(trap_exit, true),
    put('$ancestors', [Parent]).

start(#pi{} = PI) ->
    X = spawn(srv, setup_loop, [PI]),
    X ! {init, self(), []},
    X.

setup_loop(#pi{parent = Parent, mod = Mod, name = Name,
               hibernate = Hibernate} =
               PI) ->
    setup(Parent, Name),
    loop(Parent, {local, srv}, PI, Mod, Hibernate).

loop(Parent, Name, State, Mod, hibernate) ->
    erlang:hibernate(?MODULE,
                     ?FUNCTION_NAME,
                     [Parent, Name, State, Mod]);
loop(Parent, Name, State, Mod, Time) ->
    server_loop(loop(Time), {Parent, Name, State, Mod}).

loop(Parent, Name, State, Mod) ->
    server_loop(loop(), {Parent, Name, State, Mod}).

loop() -> receive Input -> Input end.

loop(Timeout) ->
    receive
        Input -> Input after Timeout -> {timeout, [], []}
    end.

reply({To, Tag}, Reply) -> To ! {Tag, Reply}.

call(Fun, Mod, Message, Sender, State) ->
    case Fun of
        info -> Mod:server(Message, Sender, State);
        X when X == init; X == terminate ->
            Mod:server(Fun, Sender, State)
    end.

server_loop({Fun, Sender, Message},
            {Parent, _, State, Mod} = Attr) ->
    try dispatch(call(Fun, Mod, Message, Sender, State),
                 Sender,
                 Attr)
    catch
        Error:Reason:Stack ->
            Crash = {Error, Reason, Stack},
            Parent ! {crash, Crash},
            Mod:terminate(Crash, Sender, State),
            io:format("Exception: ~p~n", [Crash])
    end;
server_loop(Msg, Attr) ->
    server_loop({info, {self(), []}, Msg}, Attr).

dispatch(Call, Sender, {Parent, Name, _, Mod}) ->
    Time = infinity,
    case Call of
        {stop, _, _} -> ok;
        {stop, Reply, F, _} ->
            reply(F, Reply),
            Reply;
        {ok, Reply, State} ->
            reply(Sender, Reply),
            loop(Parent, Name, State, Mod, Time);
        {ok, State} -> loop(Parent, Name, State, Mod, Time)
    end.

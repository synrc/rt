-module(srv).
-compile('PI OTP GenServer /w Persistence').
-compile(export_all).
-include("streams.hrl").

name(App) ->
    list_to_atom(lists:concat([otp,App])).

emulate_otp(Parent,App) ->
    catch unregister(name(App)),
    catch register(name(App),self()),
    process_flag(trap_exit, true),
    put('$ancestors', [Parent]),
    put('$initial_call', {application_controller, start, 1}).

% gen_server ctor

init(#gen_server{app=App,parent=Parent}=Server) ->
    emulate_otp(Parent,App),
    loop(Parent,{local,?MODULE}, Server, ?MODULE, infinity);
init({Parent, App, N, PredN, {Len,Msg}}) ->
    emulate_otp(Parent,App),
    T = erlang:monotonic_time(milli_seconds),
    Server = #gen_server{file=otp:open(App),parent=Parent,app=App,
                    acc_pred=PredN,acc=N,acc_len=0,state=[],msg=Msg,
                    len=Len,circa= <<>>,time=T, init=T,sign= <<>>},
    loop(Parent, {local, ?MODULE}, Server, ?MODULE, infinity).

% gen_server bootstrap

loop(Parent, Name, State, Mod, hibernate) -> erlang:hibernate(?MODULE,hibernate,[Parent, Name, State, Mod]);
loop(Parent, Name, State, Mod, Time)      -> server(drain(Time),        Parent, Name, State, Mod).
hibernate(Parent, Name, State, Mod)       -> server(drain(),            Parent, Name, State, Mod).
drain()                                   -> receive Input -> Input end.
drain(Timeout)                            -> receive Input -> Input after Timeout -> {timeout,[],[]} end.
reply({To,Tag},Reply)                     -> To ! {Tag,Reply}.

% gen_server protocol

server({Fun, Sender, Msg}, P, N, S, M) ->
     try dispatch(call(Fun,Msg,Sender,S),Sender,P,N,M)
   catch Error:Reason ->
         io:format("Exception: ~p",[{{Error,Reason},
         erlang:get_stacktrace()}]) end;

server(Msg,P,N,S,M)    ->
   server({'$gen_cast', {self(),[]}, Msg}, P, N, S, M).

call(Fun,Msg,Sender,S) ->
    case Fun of
         'init'        -> otp:'init'(Msg, Sender, S);
         'EXIT'        -> otp:'EXIT'(Msg, Sender, S);
         'system'      -> otp:'system'   (Msg, Sender, S);
         'timeout'     -> otp:'timeout'  (Msg, Sender, S);
         '$gen_call'   -> otp:'$gen_call'(Msg, Sender, S);
         '$gen_cast'   -> otp:'$gen_cast'(Msg, Sender, S) end.

dispatch(Call,Sender,P,N,M)   ->
    T = infinity,
    case Call of
	     {stop,R,F}    -> ok;
	     {stop,R,F,S}  -> reply(F,R), R;
         {ok,R,S}      -> reply(Sender,R),
                          loop(P,N,S,M,T);
         {ok,S}        -> loop(P,N,S,M,T) end.


% server

server(Msg,Sender,#gen_server{acc=N}=Server) when N >= ?LIMIT ->
    {stop, normal, Server};

server(Msg,Sender,#gen_server{acc=N,len=C,acc_len=AccLen}=Server) when AccLen > C ->
    spawn(srv, init, [ otp:flush(Msg,Sender,Server#gen_server{acc=N+1}) ]),
    {stop, normal, Server};

server(Msg,Sender,#gen_server{acc=N,circa=X,acc_len=AccLen,sign=Sign}=Server) ->
    {Y, Len, S} = otp:append(X,{Msg,Sender},Sign,N),
    otp:gc(),
    {ok, [], Server#gen_server{acc_len=AccLen+Len,acc=N+1,circa=Y,sign=S}}.


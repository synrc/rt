-module(otp).
-description('PI OTP Instance for File BackEnd').
-compile(export_all).
-include("streams.hrl").

block_size() -> 100000000.
msg_size()   -> 10000.
amount()     -> ?LIMIT + 100.

launch(App,N,PredN,Len) ->
  spawn(srv, init, [ {self(),App,N,PredN,Len} ]).

% file ops
gc() -> [ erlang:garbage_collect(Pid) || Pid <- erlang:processes() ].

open(App) -> {ok, F} = file:open(lists:concat([App]), [raw, binary, append, read, write]), F.
empty_append(App,X) -> ok.
pure_append(App,X) -> F = open(App), file:write(F,X), file:close(F), gc().
append(App,X) -> spawn(fun() -> empty_append(App,X) end).

% on disk chunk writer

flush(Msg,Sender,#gen_server{file=F,circa=X,acc_len=AccLen,app=App,
                             acc_pred=PredN,acc=N,time=T1,init=T0}=Server) ->
    append(App,X),
    T2 = erlang:monotonic_time(milli_seconds),
    NewAccLen = round(AccLen/(T2/1000-T1/1000)),
%    NewAccLen = 220000000, % disable variator
    spawn (fun() -> io:format("~p: ~p: rate ~p MB/s messages ~p in ~p/~p sec~n",
       [self(),srv:name(App),round(NewAccLen/1000000),N-PredN,round(T2/1000-T1/1000),round(T2/1000-T0/1000)]) end),
    Server#gen_server{acc_len=0,acc_pred=N,acc=N+1,len=NewAccLen,time=T2}.

% OTP GenServer Impl

'$gen_call'(X,Y,Z) ->
    srv:server(X,Y,Z).

'$gen_cast'(X,Y,Z) ->
    case srv:server(X,Y,Z) of
         Stop = {stop,_,_} -> Stop;
         Okey = {ok,_,Sta} -> {ok,Sta} end.

'init'(Msg,Sender,State) ->
    {ok, State}.

'EXIT'(Msg,Sender,State) ->
    %io:format("EXIT: ~p~n",[{Msg,Sender,State}]),
    {stop, Msg, {Sender,[]}, State}.

timeout(Msg,Sender,State) ->
    %io:format("timeout: ~p~n",[{Msg,Sender,State}]),
    {stop, timeout, {Sender,[]}, State}.

system(Msg,Sender,State) ->
    %io:format("system: ~p~n",[{Msg,Sender,State}]),
    {stop, Msg, {Sender,[]}, State}.

% client

observe(N) -> observer:start(), timer:sleep(2000), 
              streams(N).
streams(N) -> [ begin start(I), timer:sleep(250) end || I <- lists:seq(1,N) ].

start(App) ->
    file:delete(lists:concat([App])),
    start(App,1,0,{block_size(),msg_size()}).
start(App,N,PredN,{Len,Msg}) ->
    spawn_link(fun() ->
        launch(App,N,PredN,{Len,Msg}),
        io:format("whereis writer: ~p~n",[self()]),
        test_pid(App) end).

test_pid(App) ->
    {Timer,_} = timer:tc(fun() ->
                      Loop = fun L(0) -> ok;
                                 L(N) -> try
%        gen_server:call(whereis(srv:name(App)),binary:copy(<<"1">>,msg_size()))
         whereis(srv:name(App)) ! binary:copy(<<"1">>,otp:msg_size())
                                         catch E:R-> retry_not_implemented end,
                                         L(N-1) end,
                      Loop(amount()),
                      ok end),
    T = Timer/1000000,
    io:format("~p messages sent in ~ps with ~p m/s rate~n",[amount(),round(T),trunc(amount()/T)]),
    ok.

secret()                  -> <<"ThisIsClassified">>.
append(Circ,Record,SHA,N) -> signature(SHA,term_to_binary(Record),Circ,N).
sign(SHA,Bin,Type)        -> crypto:hmac(Type,secret(),<<SHA/binary,Bin/binary>>).
signature(SHA,Bin,Circ,N) -> {[Bin|Circ], size(Bin), <<>>}.
%    case application:get_env(streams,signature,none) of
%         none -> {[Bin|Circ], size(Bin), <<>>};
%         Type -> Sign = sign(SHA,Bin,Type),
%                 {[<<Bin/binary,Sign/binary>>|Circ], size(Bin), Sign} end.

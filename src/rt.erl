-module(rt).

-include_lib("rt/include/res.hrl").

-include_lib("rt/include/pi.hrl").

-behaviour(application).

-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_, _) ->
    supervisor:start_link({local, rt}, rt, []).

stop(_) -> ok.

init([]) -> {ok, {{one_for_one, 1000, 10}, []}}.

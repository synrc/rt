-ifndef(PI_HRL).
-define(PI_HRL, true).

-record(pi, { name = 1 :: term(),
              tab = [] :: term(),
              sup = rt :: atom(),
              mod = srv :: atom(),
              parent = self() :: pid(),
              hibernate = infinity :: hibernate | infinity | integer(),
              state = [] :: term() }).

-endif.

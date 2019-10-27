-ifndef(N2O_HRL).
-define(N2O_HRL, true).

-record(pi, { name     :: term(),
              table    :: atom(),
              sup      :: atom(),
              module   :: atom(),
              state    :: term()  }).

-endif.

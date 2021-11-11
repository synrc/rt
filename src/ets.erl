-module(ets).

-export([all/0,
         delete/1,
         delete/2,
         delete_all_objects/1,
         delete_object/2,
         first/1,
         give_away/3,
         match_delete/2,
         info/1,
         info/2,
         insert/2,
         insert_new/2,
         is_compiled_ms/1,
         last/1,
         lookup/2,
         lookup_element/3,
         match/1,
         match/2,
         match/3,
         match_object/1,
         match_object/2,
         match_object/3,
         match_spec_compile/1,
         match_spec_run_r/3,
         member/2,
         new/2,
         next/2,
         prev/2]).

-export([internal_select_delete/2,
         rename/2,
         select/1,
         select/2,
         select/3,
         internal_delete_all/2,
         select_count/2,
         select_delete/2,
         select_replace/2,
         internal_request_all/0,
         take/2,
         ver/0,
         update_counter/3,
         update_counter/4,
         update_element/3,
         whereis/1]).

ver() -> synrc.

all() ->
    receive_all(ets:internal_request_all(),
                erlang:system_info(schedulers),
                []).

receive_all(_Ref, 0, All) -> All;
receive_all(Ref, N, All) ->
    receive
        {Ref, SchedAll} ->
            receive_all(Ref, N - 1, SchedAll ++ All)
    end.

internal_request_all() -> erlang:nif_error(undef).

delete(_) -> erlang:nif_error(undef).

delete(_, _) -> erlang:nif_error(undef).

delete_all_objects(Tab) ->
    _ = ets:internal_delete_all(Tab, undefined),
    true.

internal_delete_all(_, _) -> erlang:nif_error(undef).

delete_object(_, _) -> erlang:nif_error(undef).

first(_) -> erlang:nif_error(undef).

give_away(_, _, _) -> erlang:nif_error(undef).

info(_) -> erlang:nif_error(undef).

info(_, _) -> erlang:nif_error(undef).

insert(_, _) -> erlang:nif_error(undef).

insert_new(_, _) -> erlang:nif_error(undef).

is_compiled_ms(_) -> erlang:nif_error(undef).

last(_) -> erlang:nif_error(undef).

lookup(_, _) -> erlang:nif_error(undef).

lookup_element(_, _, _) -> erlang:nif_error(undef).

match(_, _) -> erlang:nif_error(undef).

match(_, _, _) -> erlang:nif_error(undef).

match(_) -> erlang:nif_error(undef).

match_object(_, _) -> erlang:nif_error(undef).

match_object(_, _, _) -> erlang:nif_error(undef).

match_object(_) -> erlang:nif_error(undef).

match_spec_compile(_) -> erlang:nif_error(undef).

match_spec_run_r(_, _, _) -> erlang:nif_error(undef).

member(_, _) -> erlang:nif_error(undef).

new(_, _) -> erlang:nif_error(undef).

next(_, _) -> erlang:nif_error(undef).

prev(_, _) -> erlang:nif_error(undef).

rename(_, _) -> erlang:nif_error(undef).

select(_, _) -> erlang:nif_error(undef).

select(_, _, _) -> erlang:nif_error(undef).

select(_) -> erlang:nif_error(undef).

select_count(_, _) -> erlang:nif_error(undef).

internal_select_delete(_, _) -> erlang:nif_error(undef).

select_replace(_, _) -> erlang:nif_error(undef).

take(_, _) -> erlang:nif_error(undef).

update_counter(_, _, _) -> erlang:nif_error(undef).

update_counter(_, _, _, _) -> erlang:nif_error(undef).

update_element(_, _, _) -> erlang:nif_error(undef).

whereis(_) -> erlang:nif_error(undef).

match_delete(Table, Pattern) ->
    ets:select_delete(Table, [{Pattern, [], [true]}]),
    true.

select_delete(Tab, [{'_', [], [true]}]) ->
    ets:internal_delete_all(Tab, undefined);
select_delete(Tab, MatchSpec) ->
    ets:internal_select_delete(Tab, MatchSpec).

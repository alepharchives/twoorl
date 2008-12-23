-module(twoorl.ui_msgs_controller).

-import(lists).

-export([index/2, index/3, private/0]).
-include("twoorl.hrl").

private() ->
    true.

index(A, Errs) ->
    index(A, Errs, []).

index(A, Errs, Msgs) ->
    B = util:get_bundle(A),
    Msgs1 = lists:map(B, Msgs),
    Errs1 = lists:map(B, Errs),
    Errs2 = [{error, Err} || Err <- Errs1],
    {data, Msgs1 ++ Errs2}.

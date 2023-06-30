-module(playground).
-export([fn/1]).

fn(p) ->
    G = fun (q) -> p*q end,
    G(3).

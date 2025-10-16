-module(server).
-export([start/1]).

start(N) ->
    spawn(fun() -> init(N) end).

init(N) ->
    Store = store:new(N),
    Validator = validator:start(),
    server(Validator, Store).

server(Validator, Store) ->
    receive
        {open, Client} ->
            Client ! {transaction, Validator, Store} %% Send validator and store information for Transaction Handler creation at Client
            server(Validator, Store);
        stop ->
            Validator ! stop,
            store:stop(Store)
    end.

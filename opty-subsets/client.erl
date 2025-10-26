-module(client).
-export([start/4]).

start(ClientID, Entries, Server, SubsetLen) ->
    Subset = lists:sublist([X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- lists:seq(1, Entries)])], SubsetLen),
    spawn(fun() -> open(ClientID, Subset, Server, 0, 0) end).

open(ClientID, Subset, Server, Total, Ok) ->
    Server ! {open, self()},
    receive
        {stop, From} ->
            io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
            [ClientID, Total, Ok, 100*Ok/Total]),
            From ! {done, self()},
            ok;
        {transaction, Validator, Store} ->
            Handler = handler:start(self(), Validator, Store),
            case do_transaction(ClientID, Subset, Handler) of
                ok ->
                    open(ClientID, Subset, Server, Total+1, Ok+1);
                abort ->
                    open(ClientID, Subset, Server, Total+1, Ok)
            end
    end.

do_transaction(_, [], Handler) ->
    do_commit(Handler);
do_transaction(ClientID, Subset, Handler) ->
    [Num | RemSubset] = Subset,
    Op = rand:uniform(),
    if Op >= 0.5 ->
         do_read(Num, Handler);
       true -> 
         do_write(Num, Handler, ClientID)
    end,
    do_transaction(ClientID, RemSubset, Handler).

do_read(Num, Handler) ->
    Ref = make_ref(),
    Handler ! {read, Ref, Num},
    receive
        {value, Ref, Value} -> Value
    end.

do_write(Num, Handler, Value) ->
    Handler ! {write, Num, Value}.

do_commit(Handler) ->
    Ref = make_ref(),
    Handler ! {commit, Ref},
    receive
        {Ref, Value} -> Value
    end.


    

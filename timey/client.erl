-module(client).
-export([start/4]).

start(ClientID, Entries, Server, SubsetLen) ->
    Subset = lists:sublist([X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- lists:seq(1, Entries)])], SubsetLen),
    spawn(fun() -> open(ClientID, Subset, Server, 0, 0) end).

open(ClientID, Subset, Server, Total, Ok) ->
    Server ! {open, self()},
    receive
        {stop, From} ->
            %% io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n", [ClientID, Total, Ok, 100*Ok/Total]),
            io:format("=~w/~w;", [Ok, Total]),
            From ! {done, self()},
            ok;
        {transaction, Time, Store} ->
            Tref = make_ref(),
            Handler = handler:start(self(), Tref, Time, Store),
            case do_transaction(ClientID, Subset, Handler, Tref) of
                ok ->
                    open(ClientID, Subset, Server, Total+1, Ok+1);
                abort ->
                    open(ClientID, Subset, Server, Total+1, Ok)
            end
    end.

do_transaction(_, [], Handler, Tref) ->
    do_commit(Handler, Tref);
do_transaction(ClientID, Subset, Handler, Tref) ->
    [Num | RemSubset] = Subset,
    Op = rand:uniform(),
    if
        Op >= 0.5 ->
            case do_read(Num, Handler, Tref) of
                abort -> abort;
                _ -> do_transaction(ClientID, RemSubset, Handler, Tref)
            end;
        true ->
            case do_write(Num, Handler, ClientID, Tref) of
                abort -> abort;
                ok -> do_transaction(ClientID, RemSubset, Handler, Tref)
            end
    end.
%% do_transaction(ClientID, Entries, 0, Writes, Handler, Tref) ->
%%     case do_write(Entries, Handler, ClientID, Tref) of
%%         abort ->
%%             abort;
%%         ok ->
%%             do_transaction(ClientID, Entries, 0, Writes-1, Handler, Tref)
%%     end;
%% do_transaction(ClientID, Entries, Reads, 0, Handler, Tref) ->
%%     case do_read(Entries, Handler, Tref) of
%%         abort ->
%%             abort;
%%         _ ->
%%             do_transaction(ClientID, Entries, Reads-1, 0, Handler, Tref)
%%     end;
%% do_transaction(ClientID, Entries, Reads, Writes, Handler, Tref) ->
%%     Op = rand:uniform(),
%%     if Op >= 0.5 ->
%%          case do_read(Entries, Handler, Tref) of
%%              abort ->
%%                  abort;
%%              _ ->
%%                  do_transaction(ClientID, Entries, Reads-1, Writes, Handler, Tref)
%%          end;
%%        true ->
%%          case do_write(Entries, Handler, ClientID, Tref) of
%%              abort ->
%%                  abort;
%%              ok ->
%%                  do_transaction(ClientID, Entries, Reads, Writes-1, Handler, Tref)
%%          end
%%     end.

do_read(Num, Handler, Tref) ->
    Ref = make_ref(),
    Handler ! {read, Ref, Num},
    receive
        {value, Ref, {ok, Value}} ->
            Value;
        {abort, Tref} ->
            abort
    end.

do_write(Num, Handler, Value, Tref) ->
    Ref = make_ref(),
    Handler ! {write, Ref, Num, Value},
    receive
        {value, Ref, ok} ->
            ok;
        {abort, Tref} ->
            abort
    end.

do_commit(Handler, Tref) ->
    Handler ! commit,
    receive
        {commit, Tref} ->
            ok;
        {abort, Tref} ->
            abort
    end.


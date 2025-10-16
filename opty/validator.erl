-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} ->
            Tag = make_ref(),
            send_read_checks(Reads, Tag),  % Send check message to all entries
            case check_reads(length(Reads), Tag) of  % Check receiving messages from entries to see if the read operations have the same timestamps
                ok ->
                    update(Writes),  % Update store with pending write operations
                    Client ! {Ref, ok};
                abort ->
                    % If 1 entry or more report an abort message, we are done and send the client an abort message as well
                    Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.

update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) ->
                  % Send new Value to write in Entry
                    Entry ! {write, Value}
                  end,
                  Writes).

send_read_checks(Reads, Tag) ->
    Self = self(),
    lists:foreach(fun({Entry, Time}) ->
                  % Send read check messages to all Entries
                    Entry ! {check, Tag, Time, Self}
                  end,
                  Reads).

check_reads(0, _) ->
    ok;
check_reads(N, Tag) ->
    receive
        {Tag, ok} ->
            check_reads(N-1, Tag);
        {Tag, abort} ->
            abort
    end.

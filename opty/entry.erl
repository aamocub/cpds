-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
        {read, Ref, From} ->
            % Send ok message back to requester. Tagged with reference, pid, value and timestamp.
            From ! {{Ref, self(), Value, Time}, ok},
            entry(Value, Time);
        {write, New} ->
            entry(New , make_ref());  % Write new value to entry and update timestamp
        {check, Ref, Readtime, From} ->
            if
                Readtime == Time -> From ! {Ref, ok};
                true -> From ! {Ref, abort}
            end,
            entry(Value, Time);
        stop ->
            ok
    end.

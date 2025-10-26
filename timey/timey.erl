-module(timey).
-export([start/4, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, SubsetSize, Time) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, SubsetSize),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w SETSZ, DURATION ~w s~n",
         [Clients, Entries, SubsetSize, Time]),
    timer:sleep(Time*1000),
    stop(L).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    s ! stop,
    io:format("Stopped~n"),
    init:stop().

startClients(0, L, _, _) -> L;
startClients(Clients, L, Entries, SubsetSize) ->
    Pid = client:start(Clients, Entries, s, SubsetSize),
    startClients(Clients-1, [Pid|L], Entries, SubsetSize).

stopClients([]) ->
    ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    stopClients(L).

waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.

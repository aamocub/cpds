-module(remote_opty).
-export([start/5, stop/1, startServer/2]).
-define(SERVER_HOST, 'opty-srv@localhost').
-define(START_TIMEOUT, 10000).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, Reads, Writes, Time) ->
    spawn(?SERVER_HOST, remote_opty, startServer, [self(), Entries]),
    waitServer(),
    io:format("Starting clients at ~w...~n", [self()]),
    L = startClients(Clients, [], Entries, Reads, Writes),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n",
         [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time*1000),
    stop(L).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    { server, ?SERVER_HOST } ! stop,
    io:format("Stopped~n"),
    init:stop().

startClients(0, L, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes) ->
    Pid = client:start(Clients, Entries, Reads, Writes, ({server, ?SERVER_HOST})),
    io:format("Client started: ~w~n", [Pid]),
    startClients(Clients-1, [Pid|L], Entries, Reads, Writes).

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

startServer(CallerPid, Entries) ->
    Pid = server:start(Entries),
    register(server, Pid),
    CallerPid ! {started, Pid}.

waitServer() ->
    receive
        {started, Pid} ->
            io:format("Server started: ~w~n", [Pid]),
            ok
    after ?START_TIMEOUT ->
        io:format("Server failed to start~n"),
        abort
    end.
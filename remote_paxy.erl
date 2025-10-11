-module(remote_paxy).
-export([start/1, stop/0, stop/1, start_bridge/3]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

-define(ACCEPTOR_HOST, 'paxy-acc@DESKTOP-RB1DUQF').
-define(PROPOSER_HOST, 'paxy-pro@DESKTOP-RB1DUQF').

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
  AccRegister = [homer, marge, bart, lisa, maggie],
  PropRegister = [fry, bender, leela],
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      spawn(?ACCEPTOR_HOST, fun() -> start_acceptors(AccIds, AccRegister) end),
      start_bridges(PropRegister, ?ACCEPTOR_HOST, ?PROPOSER_HOST),
      start_bridges(AccRegister, ?PROPOSER_HOST, ?ACCEPTOR_HOST),
      spawn(?PROPOSER_HOST, fun() ->
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End - Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.

start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId | Rest] ->
      [RegName | RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId | Rest] ->
      [{RegName, Colour} | RestInfo] = PropInfo,
      [FirstSleep | RestSleep] = Sleep,
      register(RegName, proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main)),
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

start_bridges(Bridges, Host, TargetHost) ->
  case Bridges of
    [] ->
      ok;
    [BridgeName | Rest] ->
      spawn(Host, remote_paxy, start_bridge, [BridgeName, Host, TargetHost]),
      start_bridges(Rest, Host, TargetHost)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N - 1)
  end.

stop() ->
  stop(gui),
  Participants = [fry, bender, leela, homer, marge, bart, lisa, maggie],
  lists:foreach(
    fun(Acc) -> spawn(?ACCEPTOR_HOST, remote_paxy, stop, [Acc]) end,
    Participants
  ),
  lists:foreach(
    fun(Prop) -> spawn(?PROPOSER_HOST, remote_paxy, stop, [Prop]) end,
    Participants
  ),
  io:format("[Paxy] All components stopped~n").

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.

start_bridge(BridgeName, BridgeHost, TargetHost) ->
  io:format("[Bridge] Starting bridge ~w in ~w to ~w~n",
            [BridgeName, BridgeHost, TargetHost]),
  register(BridgeName, self()),
  bridge(BridgeName, TargetHost).

bridge(BridgeName, TargetHost) ->
  receive
    stop ->
      io:format("[Bridge] Bridge ~w stopping~n", [BridgeName]),
      ok;
    Msg ->
      {BridgeName, TargetHost} ! Msg,
      bridge(BridgeName, TargetHost)
  end.

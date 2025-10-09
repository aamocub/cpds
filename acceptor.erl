-module(acceptor).
-export([start/2]).

-define(delay, 500).
-define(drop, 0).

send2(Pid, Message) ->
  P = rand:uniform(10),
  if P =< ?drop ->
      % Drop message
      io:format("message dropped~n");
    true ->
      % send2 message with delay
      T = rand:uniform(?delay),
      timer:send_after(T, Pid, Message)
  end.

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  % Promised = order:null(),
  % Voted = order:null(),
  % Value = na,
  pers:open(Name),
  {Pr, Vt, Ac, Pn} = pers:read(Name),
  pers:close(Name),
  if
    Pn == na -> acceptor(Name, Pr, Vt, Ac, PanelId);
    true -> acceptor(Name, Pr, Vt, Ac, Pn)
  end,
  acceptor(Name, Pr, Vt, Ac, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          send2(Proposer, {promise, Round, Voted, Value}),
          pers:open(Name),
          pers:store(Name, Promised, Voted, Value, PanelId),
          pers:close(Name),
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          send2(Proposer, {sorry, {prepare, Round}}),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of % Can we vote for this ballot?
        true ->
          send2(Proposer, {vote, Round}),
          pers:open(Name),
          pers:store(Name, Promised, Round, Value, PanelId),
          pers:close(Name),
          case order:goe(Round, Voted) of % Is the ballot number higher than the current maximum one?
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              % Save the number and the value that come in the ballot
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        false ->
          send2(Proposer, {sorry, {accept, Round}}),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      pers:open(Name),
      pers:delete(Name),
      pers:close(Name),
      ok
  end.

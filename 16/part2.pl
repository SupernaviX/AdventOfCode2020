:- use_module(library(clpfd)).
:- initialization(main, main).

read_input(Filename, Constraints, MyTicket, Tickets) :-
  read_file_to_string(Filename, String, []),

  % read constraints until first double-newline
  sub_string(String, ConEnd, 2, _, "\n\n"), !,
  sub_string(String, 0, ConEnd, _, ConString),
  split_string(ConString, "\n", "", ConStrings),
  maplist(parse_constraint, ConStrings, Constraints),

  % read your ticket next
  TicketStart is ConEnd + 2,
  sub_string(String, TicketStart, _, 0, TicketString),
  sub_string(TicketString, YTEnd, 2, _, "\n\n"), !,
  YTStart is 13, % "your ticket:\n"
  YTLen is YTEnd - YTStart,
  sub_string(TicketString, YTStart, YTLen, _, YourTicket),
  parse_ticket(YourTicket, MyTicket),

  %  read nearby tickets last
  NTStart is YTEnd + 18, % "nearby tickets:\n"
  sub_string(TicketString, NTStart, _, 0, NTString),
  split_string(NTString, "\n", "", NTStrings),
  maplist(parse_ticket, NTStrings, Tickets).

parse_constraint(ConString, Constraint) :-
  split_string(ConString, ":", " ", [Term, Constraints]),
  sub_string(Constraints, FirstEnd, 4, _, " or "), !,
  sub_string(Constraints, 0, FirstEnd, _, First),
  SecStart is FirstEnd + 4,
  sub_string(Constraints, SecStart, _, 0, Second),
  parse_range(First, FirstRange),
  parse_range(Second, SecondRange),
  Constraint = constraint(Term, [FirstRange, SecondRange]).
parse_range(RangeString, Range) :-
  split_string(RangeString, "-", "", Strings),
  maplist(number_string, [Start, End], Strings),
  Range = range(Start, End).
parse_ticket(TicketString, Ticket) :-
  split_string(TicketString, ",", "", TicketStrings),
  maplist(number_string, Ticket, TicketStrings).

% actual problem starts here
satisfies(constraint(_, [range(A, B), range(C, D)]), Term) :-
  Term in A..B \/ C..D.

can_be_valid([Constraint|_], Term) :- satisfies(Constraint, Term).
can_be_valid([_|Constraints], Term) :- can_be_valid(Constraints, Term).

is_somehow_valid(Constraints, Ticket) :-
  forall(member(Term, Ticket), can_be_valid(Constraints, Term)).

valid(Constraint, Tickets, Index) :-
  maplist(nth1(Index), Tickets, Terms),
  maplist(satisfies(Constraint), Terms).

candidates(Tickets, Max, Constraint, Candidates) :-
  numlist(1, Max, AllIndices),
  include(valid(Constraint, Tickets), AllIndices, Candidates).

satisfy(AllCandidates, Solution) :-
  satisfy(AllCandidates, [], Pairs),
  order_pairs(Pairs, 1, Solution), !.

satisfy(AllCandidates, Seen, []) :-
  length(AllCandidates, CanLen),
  length(Seen, SeenLen),
  SeenLen = CanLen.
satisfy(AllCandidates, Seen, [Solution|Rest]) :-
  length(Seen, PrevIndex),
  Index is PrevIndex + 1,
  % use prolog magic to find the element with Seen.length items
  nth1(SolutionIndex, AllCandidates, Candidates),
  length(Candidates, Index),
  % store that and recurse
  subtract(Candidates, Seen, [SolutionValue]),
  Solution = pair(SolutionIndex, SolutionValue),
  append(Seen, [SolutionValue], NextSeen),
  satisfy(AllCandidates, NextSeen, Rest).

order_pairs([], _, []).
order_pairs(Pairs, OldIndex, [NewIndex|Rest]) :-
  member(Pair, Pairs),
  Pair = pair(NewIndex, OldIndex),
  NextOldIndex is OldIndex + 1,
  subtract(Pairs, [Pair], NextPairs),
  order_pairs(NextPairs, NextOldIndex, Rest).

nth1_(List, Index, Elem) :- nth1(Index, List, Elem).
solve(Constraints, Tickets, Solution) :-
  length(Constraints, Count),
  maplist(candidates(Tickets, Count), Constraints, AllCandidates),
  satisfy(AllCandidates, SolutionOrder),
  maplist(nth1_(Constraints), SolutionOrder, Solution).

in_result(constraint(Name, _)) :- sub_string(Name, 0, 9, _, "departure").
answer([], [], 1).
answer([Constraint|Constraints], [Term|Terms], Answer) :-
  answer(Constraints, Terms, NextAnswer),
  ( in_result(Constraint) -> Answer is NextAnswer * Term ; Answer is NextAnswer ).

main(_) :-
  read_input("input", Constraints, MyTicket, Tickets),
  include(is_somehow_valid(Constraints), Tickets, ValidTickets),
  solve(Constraints, ValidTickets, Solution),
  answer(Solution, MyTicket, Answer),
  writeln(Answer).
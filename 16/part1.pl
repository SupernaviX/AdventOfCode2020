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

sum([], 0).
sum([X|Xs], Sum) :-
  sum(Xs, Rest),
  Sum is X + Rest.

sum_of_invalid(Constraints, Ticket, Answer) :-
  exclude(can_be_valid(Constraints), Ticket, InvalidTerms),
  sum(InvalidTerms, Answer).

solve(_, [], 0).
solve(Constraints, [Ticket|Tickets], Answer) :-
  sum_of_invalid(Constraints, Ticket, PartialAnswer),
  solve(Constraints, Tickets, RestOfAnswer),
  Answer is PartialAnswer + RestOfAnswer.

main(_) :-
  read_input("input", Constraints, _, Tickets),
  solve(Constraints, Tickets, Answer),
  writeln(Answer).
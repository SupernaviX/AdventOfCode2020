%% -*- erlang -*-
-module(solution).
-export([main/1]).

read_input(Filename) ->
  {ok, Device} = file:open(Filename, [read]),
  try read_line(Device)
    after file:close(Device)
  end.
read_line(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> [string:trim(Line)] ++ read_line(Device)
  end.

parse_input(Lines) ->
  parse_input(Lines, #{}).
parse_input([""|Lines], RuleMap) ->
  { RuleMap, Lines };
parse_input([Line|Lines], RuleMap) ->
  { Index, Rule } = parse_rule(Line),
  NewRuleMap = RuleMap #{ Index => Rule },
  parse_input(Lines, NewRuleMap).

parse_rule(String) ->
  [RawIndex, RawBody] = string:split(String, ": "),
  Index = list_to_integer(RawIndex),
  Rule = case string:split(RawBody, " | ") of
    [ Clause1, Clause2 ] -> { oneOf, parse_rule_clause(Clause1), parse_rule_clause(Clause2) };
    [ Clause ] -> parse_rule_clause(Clause)
  end,
  { Index, Rule }.
parse_rule_clause([ $", Char, $"]) ->
  { terminal, Char };
parse_rule_clause(Clause) ->
  lists:map(
    fun(Str) -> list_to_integer(Str) end,
    string:split(Clause, " ", all)).

make_interesting(RuleMap) ->
  RuleMap # {
    8 => { oneOf, [42], [42, 8] },
    11 => { oneOf, [42, 31], [42, 11, 31] }
  }.

satisfies(RuleMap, Str) ->
  Result = satisfies(RuleMap, 0, Str),
  Temp = lists:member([], Result),
  % io:format("~p: ~w~n", [Str, Temp]),
  Temp.
satisfies(RuleMap, RuleId, Str) ->
  #{ RuleId := Rule } = RuleMap,
  satisfies1(RuleMap, Rule, Str).

satisfies1(_, { terminal, Str }, [Str|Suffix]) ->
  [Suffix];
satisfies1(RuleMap, [RuleId], Str) ->
  satisfies(RuleMap, RuleId, Str);
satisfies1(RuleMap, [RuleId|Rules], Str) ->
  Suffixes = satisfies(RuleMap, RuleId, Str),
  lists:merge(lists:map(
    fun(Suffix) -> satisfies1(RuleMap, Rules, Suffix) end,
    Suffixes));
satisfies1(RuleMap, { oneOf, Rule1, Rule2 }, Str) ->
  lists:merge(
    satisfies1(RuleMap, Rule1, Str),
    satisfies1(RuleMap, Rule2, Str));
satisfies1(_, _ , _) ->
  [].

solve(RuleMap, Strings) ->
  Successes = lists:filter(fun(String) -> satisfies(RuleMap, String) end, Strings),
  length(Successes).

main(_) ->
  Lines = read_input("input"),
  { RuleMap, Strings } = parse_input(Lines),
  Results = solve(RuleMap, Strings),
  io:format("Part 1: ~w ~n", [Results]),
  NewRuleMap = make_interesting(RuleMap),
  NewResults = solve(NewRuleMap, Strings),
  io:format("Part 2: ~w", [NewResults]).
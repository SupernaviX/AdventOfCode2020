function readinput(filename)
  input = readlines("input")
  midpoint = findfirst(isequal(""), input)
  p1 = map(n -> parse(Int, n), input[2 : midpoint - 1])
  p2 = map(n -> parse(Int, n), input[midpoint + 2 : end])
  [p1, p2]  
end

function combat(decks)
  queues = [copy(deck) for deck=decks]
  while !any(isempty.(queues))
    cards = [popfirst!(q) for q=queues]
    winner = queues[argmax(cards)]
    for card in sort(cards, rev=true)
      push!(winner, card)
    end
  end
  winner = argmax(length.(queues))
  winner, queues[winner]
end

function recursivecombat(decks, game = 1)
  queues = [copy(deck) for deck=decks]
  rounds = Set()
  round = 1
  while !any(isempty.(queues))
    round += 1
    if queues in rounds
      return 1, queues[1]
    end
    push!(rounds, deepcopy(queues))
    cards = [popfirst!(q) for q=queues]
    if all(cards .<= length.(queues))
      recdecks = [ queues[i][1:cards[i]] for i=1:length(queues)]
      windex, _ = recursivecombat(recdecks, game + 1)
    else
      windex = argmax(cards)
    end
    push!(queues[windex], cards[windex])

    for loser in cards[1:end .!=windex]
      push!(queues[windex], loser)
    end
  end
  winner = argmax(length.(queues))
  winner, queues[winner]
end

function deckvalue(deck)
  decklength = length(deck)
  sum([ (decklength - i + 1) * deck[i] for i=1:decklength ])
end

decks = readinput("input")

v1, d1 = combat(decks)
v1value = deckvalue(d1)
println("Part 1: $v1value")

v2, d2 = recursivecombat(decks)
v2value = deckvalue(d2)
println("Part 2: $v2value")
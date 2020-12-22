defmodule Food do
  @food_regex ~r/([[:alpha:] ]+) \(contains ([[:alpha:] ,]+)\)/
  defstruct [:ingredients, :allergens]

  def parse(string) do
    [_, i, a] = Regex.run(@food_regex, string)

    %Food{
      ingredients: MapSet.new(String.split(i, " ")),
      allergens: String.split(a, ", ")
    }
  end
end

defmodule Part1 do
  def solve(foods) do
    ingredients = safe_ingredients(foods)

    Enum.sum(
      for ingredient <- ingredients,
          food <- foods,
          MapSet.member?(food.ingredients, ingredient),
          do: 1
    )
  end

  def safe_ingredients(foods) do
    MapSet.difference(ingredients(foods), dangerous_ingredients(foods))
  end

  def ingredients(foods) do
    Enum.flat_map(foods, fn food -> food.ingredients end) |> MapSet.new()
  end

  def dangerous_ingredients(foods) do
    all_candidates = allergen_candidates(foods)
    all = for {_, candidates} <- all_candidates, i <- candidates, do: i
    MapSet.new(all)
  end

  def allergen_candidates(foods) do
    candidates = for food <- foods, a <- food.allergens, do: {a, food.ingredients}

    Enum.reduce(candidates, %{}, fn {allergen, ingredients}, c_map ->
      if Map.has_key?(c_map, allergen) do
        {_, c_map} =
          Map.get_and_update(c_map, allergen, fn current ->
            {current, MapSet.intersection(current, ingredients)}
          end)

        c_map
      else
        Map.put_new(c_map, allergen, ingredients)
      end
    end)
  end
end

defmodule Part2 do
  def solve(foods) do
    all_candidates = Part1.allergen_candidates(foods)

    narrow(all_candidates)
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(&elem(&1, 1))
    |> Enum.join(",")
  end

  def narrow(all_candidates) when map_size(all_candidates) == 0, do: []

  def narrow(all_candidates) do
    {allergen, candidates} = Enum.find(all_candidates, fn {_, c} -> MapSet.size(c) == 1 end)
    ingredient = Enum.at(candidates, 0)

    new_candidates =
      Map.new(
        for {a, cans} <- all_candidates,
            a != allergen,
            do: {a, MapSet.delete(cans, ingredient)}
      )

    [{allergen, ingredient}] ++ narrow(new_candidates)
  end
end

input = File.read!("input") |> String.split("\n")
foods = for line <- input, do: Food.parse(line)
IO.puts(Part1.solve(foods))
IO.puts(Part2.solve(foods))
defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split/1)
  end

  @scores %{"X" => 1, "Y" => 2, "Z" => 3}

  def lose(opponent), do: Map.get(%{"A" => "Z", "B" => "X", "C" => "Y"}, opponent)
  def win(opponent), do: Map.get(%{"A" => "Y", "B" => "Z", "C" => "X"}, opponent)
  def draw(opponent), do: Map.get(%{"A" => "X", "B" => "Y", "C" => "Z"}, opponent)

  def round_score(opponent, self) do
    cond do
      rem(:binary.first(self) - :binary.first(opponent), 3) == 0 -> 6
      :binary.first(opponent) == :binary.first(self) - 23 -> 3
      true -> 0
    end
  end

  def part_one(data) do
    data
    |> Enum.map(fn [o, s] -> round_score(o, s) + Map.get(@scores, s) end)
    |> Enum.sum()
  end

  def part_two(data) do
    action = %{"X" => &lose/1, "Y" => &draw/1, "Z" => &win/1}

    score = fn [o, s] ->
      condition = apply(Map.get(action, s), [o])
      round_score(o, condition) + Map.get(@scores, condition)
    end

    data
    |> Enum.map(score)
    |> Enum.sum()
  end

  def main do
    data = input("input.txt")
    IO.puts "Part one: #{part_one(data)}"
    IO.puts "Part two: #{part_two(data)}"
  end
end

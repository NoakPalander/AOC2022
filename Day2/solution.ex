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
    conv = %{"X" => "A", "Y" => "B", "Z" => "C"}

    cond do
      opponent == "A" and self == "Y" -> 6
      opponent == "B" and self == "Z" -> 6
      opponent == "C" and self == "X" -> 6
      opponent == Map.get(conv, self, 0) -> 3
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
      condition = Map.get(action, s).(o)
      round_score(o, condition) + Map.get(@scores, condition)
    end

    data
    |> Enum.map(score)
    |> Enum.sum()
  end

  def main do
    data = input("input.txt")
    part_one(data) |> IO.inspect()
    part_two(data) |> IO.inspect()
  end
end

defmodule Solution do
  def get_elves(file) do
    File.read!(file)
    |> String.split("\n\n")
    |> Enum.map(fn s -> String.split(s, "\n", trim: true) end)
    |> Enum.map(fn list -> Enum.map(list, &String.to_integer/1) end)
  end

  def most_calories(elves) do
    elves
    |> Enum.map(&Enum.sum/1)
    |> Enum.max()
  end

  def top_three(elves) do
    elves
    |> Enum.map(&Enum.sum/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.sum()
  end

  def main do
    elves = get_elves("input.txt")
    IO.puts "Part one: #{most_calories(elves)}"
    IO.puts "Part two: #{top_three(elves)}"
  end
end

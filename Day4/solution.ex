defmodule Solution do
  def to_ranges(list) do
    list
    |> Enum.map(fn x -> String.split(x, "-") |> Enum.map(&String.to_integer/1) end)
    |> Enum.map(fn [s, e] -> Enum.to_list(s..e) |> MapSet.new() end)
  end

  def input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, ","))
    |> Enum.map(&to_ranges/1)
  end

  def part_one(data) do
    data
    |> Enum.map(fn [x, y] -> MapSet.intersection(x, y) |> then(&Enum.member?([x, y], &1)) end)
    |> Enum.count(fn over -> over == true end)
  end

  def part_two(data) do
    data
    |> Enum.map(fn [x, y] -> MapSet.intersection(x, y) end)
    |> Enum.count(fn range -> not Enum.empty?(range) end)
  end

  def main do
    data = input("input.txt")
    IO.puts "Part one: #{part_one(data)}"
    IO.puts "Part two: #{part_two(data)}"
  end
end

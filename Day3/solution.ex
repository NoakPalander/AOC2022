defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end

  def priority([key]) when key in ?A..?Z, do: key - ?A + 27
  def priority([key]) when key in ?a..?z, do: key - ?a + 1

  def part_one(data) do
    data
    |> Enum.map(&Enum.split(&1, div(length(&1), 2)))
    |> Enum.map(fn {fst, snd} -> Enum.filter(fst, &Enum.member?(snd, &1)) end)
    |> Enum.map(&Enum.uniq/1)
    |> Enum.map(&priority/1)
    |> Enum.sum()
  end

  def part_two(data) do
    data
    |> Enum.map(&MapSet.new(&1))
    |> Enum.chunk_every(3)
    |> Enum.map(fn group -> Enum.reduce(group, &MapSet.intersection/2) end)
    |> Enum.map(fn keylist -> keylist |> MapSet.to_list() |> priority() end)
    |> Enum.sum()
  end

  def main do
    data = input("input.txt")
    IO.puts("Part one: #{part_one(data)}")
    IO.puts("Part two: #{part_two(data)}")
  end
end

defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.trim_trailing("\n")
    |> String.to_charlist()
  end

  def look(data, length, offset \\ 0) do
    sliced = Enum.slice(data, offset, length)
    if sliced == Enum.uniq(sliced) do
      offset + length
    else
      look(data, length, offset + 1)
    end
  end

  def part_one(data), do: look(data, 4)
  def part_two(data), do: look(data, 14)

  def main do
    data = input("input.txt")
    IO.puts "Part one: #{part_one(data)}"
    IO.puts "Part two: #{part_two(data)}"
  end
end

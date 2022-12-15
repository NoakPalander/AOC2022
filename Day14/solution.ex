defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.flat_map(fn str ->
      str
      |> String.split(" -> ", trim: true)
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.flat_map(&make_line/1)
    end)
    |> Enum.uniq()
  end

  def make_line([first, last]) do
    [fx, fy] = String.split(first, ",") |> Enum.map(&String.to_integer/1)
    [lx, ly] = String.split(last, ",") |> Enum.map(&String.to_integer/1)

    if fx - lx == 0 do
      fy..ly |> Enum.map(&{fx, &1})
    else
      fx..lx |> Enum.map(&{&1, fy})
    end
  end

  def valid(formation, pos), do: not Enum.member?(formation, pos)

  def move(formation, ymax, c = {x, y} \\ {500, 0}) do
    cond do
      y == ymax -> {:invalid, c}
      valid(formation, {x, y + 1}) -> move(formation, ymax, {x, y + 1})
      valid(formation, {x - 1, y + 1}) -> move(formation, ymax, {x - 1, y + 1})
      valid(formation, {x + 1, y + 1}) -> move(formation, ymax, {x + 1, y + 1})
      true -> {x, y}
    end
  end

  def part_one(formation, ymax, count \\ 0) do
    case move(formation, ymax) do
      {:invalid, {_, _}} -> count
      next -> part_one(MapSet.put(formation, next), ymax, count + 1)
    end
  end

  def part_two(formation, ymax, count \\ 1) do
    case move(formation, ymax) do
      {500, 0} -> count
      {:invalid, n} -> part_two(MapSet.put(formation, n), ymax, count + 1)
      n -> part_two(MapSet.put(formation, n), ymax, count + 1)
    end
  end

  def main do
    formation = input("input.txt") |> MapSet.new()
    ymax = MapSet.to_list(formation) |> Enum.map(&elem(&1, 1)) |> Enum.max()

    IO.puts "Part one: #{part_one(formation, ymax)}"
    IO.puts "Part two: #{part_two(formation, ymax + 1)}"
  end
end

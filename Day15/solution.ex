defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(&parse/1)
  end

  def parse(str) do
    [sx, sy, bx, by] =
      Regex.scan(
        ~r/Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)/,
        str,
        capture: :all_but_first
      )
      |> hd()
      |> Enum.map(&String.to_integer/1)

    %{
      sensor: {sx, sy},
      beacon: {bx, by},
      distance: manhattan({sx, sy}, {bx, by})
    }
  end

  def manhattan({sx, sy}, {bx, by}), do: abs(sx - bx) + abs(sy - by)

  def circle({x, y}, r), do: Enum.map(0..r, &({x + &1, y + r - &1}))

  def excluded?(xy, data) do
    Enum.any?(data, fn %{sensor: s, distance: d} -> manhattan(xy, s) <= d end)
  end

  def part_one(data, level) do
    min_x = Enum.map(data, fn %{sensor: {x, _}, distance: d} -> x - d end) |> Enum.min()
    max_x = Enum.map(data, fn %{sensor: {x, _}, distance: d} -> x + d end) |> Enum.max()

    beacons =
      data
      |> Enum.map(&Map.get(&1, :beacon))
      |> Enum.filter(&(elem(&1, 1) == level))
      |> Enum.uniq()
      |> Enum.count()

    excluded =
      min_x..max_x
      |> Enum.map(&excluded?({&1, level}, data))
      |> Enum.count(&Quark.id/1)

    excluded - beacons
  end

  def part_two(data, level) do
    {x, y} =
      Enum.flat_map(data, fn %{sensor: s, distance: d} -> circle(s, d + 1) end)
      |> Enum.filter(fn {x, y} -> x in 0..level and y in 0..level end)
      |> Enum.uniq()
      |> Enum.find(&(not excluded?(&1, data)))

    x * level + y
  end

  def main do
    data = input("input.txt")
    IO.puts "Part one: #{part_one(data, 2_000_000)}"
    IO.puts "Part two: #{part_two(data, 4_000_000)}"
  end
end

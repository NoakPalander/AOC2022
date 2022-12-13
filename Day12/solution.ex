defmodule Solution do
  def input(filename) do
    map =
      File.read!(filename)
      |> String.split("\n", trim: true)
      |> Enum.map(&String.to_charlist/1)
      |> Enum.with_index()
      |> Enum.flat_map(fn {row, y} ->
        row
        |> Enum.with_index()
        |> Enum.map(fn {val, x} -> {{x, y}, val} end)
      end)

    elevations =
      Enum.map(map, fn
        {p, ?S} -> {p, 0}
        {p, ?E} -> {p, ?z - ?a}
        {p, v} -> {p, v - ?a}
      end)
      |> Map.new()

    {map, elevations}
  end

  def neighbours({x, y}, elevations) do
    [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
    |> Enum.filter(&(elevations[&1] <= elevations[{x, y}] + 1))
  end

  def dijkstra(distances, elevations, target) do
    case Enum.min_by(distances, &elem(&1, 1)) do
      {^target, distance} ->
        distance

      {current, distance} ->
        current
        |> neighbours(elevations)
        |> Enum.reduce(distances, fn n, ds ->
          Map.replace_lazy(ds, n, &min(&1, distance + 1))
        end)
        |> Map.delete(current)
        |> dijkstra(elevations, target)
    end
  end

  def pathway(elevations, start, goal, mapper) do
    elevations
    |> Map.keys()
    |> Map.new(&{&1, :inf})
    |> mapper.(elevations, start)
    |> dijkstra(elevations, goal)
  end

  def part_one(map, elevations) do
    start = Enum.find(map, fn {_, v} -> v == ?S end) |> elem(0)
    goal = Enum.find(map, fn {_, v} -> v == ?E end) |> elem(0)

    pathway(elevations, start, goal, fn dists, _, src ->
      Map.put(dists, src, 0)
    end)
  end

  def part_two(map, elevations) do
    start = Enum.find(map, &(elem(&1, 1) == ?S)) |> elem(0)
    goal = Enum.find(map, &(elem(&1, 1) == ?E)) |> elem(0)

    pathway(elevations, start, goal, fn ds, hs, _ ->
      Enum.filter(hs, fn {_, h} -> h == 0 end)
      |> Enum.map(&elem(&1, 0))
      |> Enum.reduce(ds, &Map.put(&2, &1, 0))
    end)
  end

  def main do
    {map, elevations} = input("input.txt")
    IO.puts("Part one: #{part_one(map, elevations)}")
    IO.puts("Part two: #{part_two(map, elevations)}")
  end
end

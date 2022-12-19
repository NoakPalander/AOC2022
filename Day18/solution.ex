defmodule Solution do
  def input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.split(",", trim: true)
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    end)
  end

  def adjacents({x, y, z}) do
    [
      {x + 1, y, z},
      {x - 1, y, z},
      {x, y + 1, z},
      {x, y - 1, z},
      {x, y, z + 1},
      {x, y, z - 1}
    ]
  end

  def exposed?(cubes, adj), do: Enum.find(cubes, &(&1 === adj)) === nil

  def bounds(cubes) do
    min = for i <- 0..2, do: (Enum.map(cubes, &elem(&1, i)) |> Enum.min()) - 1
    max = for i <- 0..2, do: (Enum.map(cubes, &elem(&1, i)) |> Enum.max()) + 1

    {min, max}
  end

  def in_bounds?(minimum, maximum, cube) do
    [minimum, maximum, Tuple.to_list(cube)]
    |> Enum.zip()
    |> Enum.all?(fn {min, max, c} -> c in min..max end)
  end

  def unvisited?(cube, cubes, visited, min, max) do
    cube not in cubes and cube not in visited and in_bounds?(min, max, cube)
  end

  def search(queue, visited, sum, cubes, min, max) do
    if Enum.empty?(queue) do
      sum
    else
      {cube, new_queue} = Qex.pop!(queue)

      if cube in visited do
        search(new_queue, visited, sum, cubes, min, max)
      else
        new_visited = [cube | visited]

        {new_sum, new_queue} =
          Enum.reduce(adjacents(cube), {sum, queue}, fn adj, {s, q} ->
            new_sum = if adj in cubes, do: s + 1, else: s

            if unvisited?(adj, cubes, new_visited, min, max) do
              {new_sum, Qex.push(q, adj)}
            else
              {new_sum, q}
            end
          end)

        search(new_queue, new_visited, new_sum, cubes, min, max)
      end
    end
  end

  def part_one(cubes) do
    Enum.reduce(cubes, 0, fn cube, acc ->
      count = adjacents(cube) |> Enum.count(&exposed?(cubes, &1))
      acc + count
    end)
  end

  def part_two(cubes) do
    {min, max} = bounds(cubes)
    initial = List.to_tuple(min)

    search(Qex.new([initial]), [], 0, cubes, min, max)
  end

  def main do
    cubes = input("input.txt")
    IO.puts("Part one: #{part_one(cubes)}")
    IO.puts("Part two: #{part_two(cubes)}")
  end
end

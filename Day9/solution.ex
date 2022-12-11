defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(fn x ->
      [dir, count] = String.split(x, " ")
      {dir, String.to_integer(count)}
    end)
  end

  def delta({hx, hy}, {tx, ty}), do: {hx - tx, hy - ty}

  def signum(x) when x < 0, do: -1
  def signum(x) when x > 0, do: 1
  def signum(x) when x == 0, do: 0

  def move_knot("R", {hx, hy}), do: {hx + 1, hy}
  def move_knot("L", {hx, hy}), do: {hx - 1, hy}
  def move_knot("U", {hx, hy}), do: {hx, hy - 1}
  def move_knot("D", {hx, hy}), do: {hx, hy + 1}

  def move_tail({tx, ty} = tail, head) do
    {dx, dy} = delta(head, tail)
    distance = :math.sqrt(dx ** 2 + dy ** 2)

    cond do
      distance in [0.0, 1.0, :math.sqrt(2)] -> tail
      true -> {tx + signum(dx), ty + signum(dy)}
    end
  end

  def move({_, 0}, head, tail, visited), do: {head, tail, visited}

  def move({dir, count}, head, tail, visited) do
    new_head = move_knot(dir, head)
    new_tail = move_tail(tail, new_head)

    move({dir, count - 1}, new_head, new_tail, [new_tail | visited])
  end

  def part_one(moves, head \\ {0, 0}, tail \\ {0, 0}, visited \\ [])
  def part_one([], _, _, visited), do: MapSet.new(visited) |> MapSet.size()

  def part_one([m | rest], head, tail, visited) do
    {h, t, v} = move(m, head, tail, visited)
    part_one(rest, h, t, v)
  end

  def move_all({_, 0}, head, tails, visited), do: {head, tails, visited}

  def move_all({dir, count}, head, [tail | tails], visited) do
    new_head = move_knot(dir, head)

    [new_tail | rest] =
      List.foldl(tails, [move_tail(tail, new_head)], fn x, acc ->
        acc ++ [move_tail(x, List.last(acc))]
      end)

    move_all({dir, count - 1}, new_head, [new_tail | rest], [List.last(rest) | visited])
  end

  def part_two(moves, head \\ {0, 0}, tails \\ List.duplicate({0, 0}, 9), visited \\ [])
  def part_two([], _, _, visited), do: MapSet.new(visited) |> MapSet.size()

  def part_two([m | rest], head, tails, visited) do
    {h, t, v} = move_all(m, head, tails, visited)
    part_two(rest, h, t, v)
  end

  def main do
    data = input("input.txt")
    IO.puts "Part one: #{part_one(data)}"
    IO.puts "Part two: #{part_two(data)}"
  end
end

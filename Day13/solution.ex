defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.split("\n\n", trim: true)
    |> Enum.flat_map(fn list ->
      list
      |> String.split("\n", trim: true)
      |> Enum.map(&(Code.eval_string(&1) |> elem(0)))
    end)
  end

  def compare(left, right) do
    cond do
      is_integer(left) and is_integer(right) -> left - right
      is_list(left) and is_integer(right) -> compare(left, [right])
      is_integer(left) and is_list(right) -> compare([left], right)
      left == [] and right == [] -> 0
      left == [] and length(right) >= 1 -> -1
      length(left) >= 1 and right == [] -> 1
      true ->
        c = compare(hd(left), hd(right))
        if c == 0, do: compare(tl(left), tl(right)), else: c
    end
  end

  def part_one(data) do
    data
    |> Enum.chunk_every(2)
    |> Enum.with_index(1)
    |> Enum.filter(fn {[left, right], _} ->
      compare(left, right) < 0
    end)
    |> Enum.map(fn {_, idx} -> idx end)
    |> Enum.sum()
  end

  def part_two(data) do
    [[[6]], [[2]] | data]
    |> Enum.sort(fn left, right -> compare(left, right) < 0 end)
    |> Enum.with_index(1)
    |> Enum.filter(fn {x, _} -> x in [[[2]], [[6]]] end)
    |> Enum.map(&(elem(&1, 1)))
    |> Enum.product()
  end

  def main do
    data = input("input.txt")
    IO.puts "Part one: #{part_one(data)}"
    IO.puts "Part two: #{part_two(data)}"
  end
end

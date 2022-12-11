defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(fn s ->
      with [inst, val] <- String.split(s, " ") do
        [inst, String.to_integer(val)]
      else
        _ -> s
      end
    end)
    |> List.flatten()
  end

  def part_one(data) do
    take_or_add = fn
      x, y when is_integer(y) -> x + y
      x, _ -> x
    end

    data
    |> Enum.with_index(1)
    |> Enum.map_reduce(1, fn
      {op, idx}, x when rem(idx, 40) == 20 -> {x * idx, take_or_add.(x, op)}
      {op, _}, x -> {0, take_or_add.(x, op)}
    end)
    |> elem(0)
    |> Enum.sum()
  end

  def lit(pos, x), do: if(pos in Range.new(x - 1, x + 1), do: '#', else: '.')

  def process([], x, _, crt), do: {x, Enum.reverse(crt)}

  def process([count | rest], x, pos, crt) when is_integer(count) do
    process(rest, x + count, pos + 1, [lit(pos, x) | crt])
  end

  def process([_ | rest], x, pos, crt), do: process(rest, x, pos + 1, [lit(pos, x) | crt])

  def part_two(data) do
    data
    |> Enum.chunk_every(40)
    |> Enum.reduce({1, []}, fn d, {x, crt} ->
      {new_x, new_crt} = process(d, x, 0, [])
      {new_x, crt ++ [new_crt]}
    end)
    |> elem(1)
    |> Enum.map(fn crt ->
      to_string(crt)
      |> String.replace("#", "█")
      |> String.replace(".", " ")
    end)
    |> Enum.join("\n")
  end

  def main do
    data = input("input.txt")
    IO.puts "Part one: #{part_one(data)}"
    IO.puts "Part two: \n#{part_two(data)}"
  end
end

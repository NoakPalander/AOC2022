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

  def eval([], _, register), do: Enum.reverse(register) |> Enum.drop(1)

  def eval([v | rest], cycle, register = [{_, top} | _]) when is_integer(v),
    do: eval(rest, cycle + 1, [{cycle, top + v} | register])

  def eval([_ | rest], cycle, register = [{_, top} | _]),
    do: eval(rest, cycle + 1, [{cycle, top} | register])

  def part_one(data) do
    eval(data, 2, [{2, 1}])
    |> Enum.flat_map(fn {idx, val} ->
      if idx in 20..220//40 do
        [idx * val]
      else
        []
      end
    end)
    |> Enum.sum()
  end

  def lit(pos, x), do: if pos in Range.new(x - 1, x + 1), do: '#', else: '.'

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

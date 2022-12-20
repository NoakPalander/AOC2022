defmodule Solution do
  @decrypt 811_589_153

  def input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def rotate(list, {0, _}), do: list

  def rotate(list, {value, _id} = item) do
    idx = Enum.find_index(list, &(&1 == item))

    new_idx =
      case Integer.mod(idx + value, length(list) - 1) do
        0 -> -1
        v -> v
      end

    list
    |> List.delete_at(idx)
    |> List.insert_at(new_idx, item)
  end

  def mix(list) do
    Enum.reduce(Range.new(0, length(list) - 1), list, fn x, acc ->
      item = Enum.find(list, fn {_, idx} -> idx == x end)
      rotate(acc, item)
    end)
  end

  def coordinate(list) do
    zero = Enum.find_index(list, &(&1 == 0))
    after_zero = fn e -> Enum.at(list, rem(zero + e, length(list))) end

    [1000, 2000, 3000] |> Enum.map(after_zero) |> Enum.sum()
  end

  def part_one(numbers) do
    numbers |> Enum.with_index() |> mix() |> Enum.map(&elem(&1, 0)) |> coordinate()
  end

  def part_two(numbers) do
    ids = numbers |> Enum.map(&(&1 * @decrypt)) |> Enum.with_index()

    0..8
    |> Enum.reduce(mix(ids), fn _, acc -> mix(acc) end)
    |> Enum.map(&elem(&1, 0))
    |> coordinate()
  end

  def main do
    numbers = input("input.txt")
    IO.puts("Part one: #{part_one(numbers)}")
    IO.puts("Part two: #{part_two(numbers)}")
  end
end

defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.map(fn str -> String.codepoints(str) |> Enum.map(&String.to_integer/1) end)
  end

  def transpose(list), do: List.zip(list) |> Enum.map(&Tuple.to_list/1)

  def view(list, n) do
    left = Enum.slice(list, Range.new(0, n - 1)) |> Enum.reverse()
    right = Enum.slice(list, Range.new(n + 1, length(list) - 1))
    [left, right]
  end

  def visible(list, height), do: Enum.filter(list, fn vec -> Enum.all?(vec, &(&1 < height)) end)

  def search(data, fun) do
    w = length(data)
    h = length(List.first(data))
    transposed = transpose(data)

    for y <- Range.new(1, w - 2) do
      for x <- Range.new(1, h - 2) do
        row = Enum.at(data, y)
        col = Enum.at(transposed, x)

        height = Enum.at(row, x)
        horizontal = view(row, x)
        vertical = view(col, y)

        fun.(height, horizontal, vertical)
      end
    end
  end

  def part_one(data) do
    w = length(data)
    h = length(List.first(data))

    inner =
      search(data, fn height, horizontal, vertical ->
        horizontal_count = visible(horizontal, height) |> length()
        vertical_count = visible(vertical, height) |> length()
        horizontal_count + vertical_count > 0
      end)

    inner_sum = Enum.map(inner, fn x -> Enum.count(x, &(&1 == true)) end) |> Enum.sum()
    outer_sum = 2 * w + 2 * h - 4
    inner_sum + outer_sum
  end

  def scenic(mat, height) do
    Enum.map(mat, fn x ->
      blocked = Enum.map(x, &(&1 < height))
      idx = Enum.find_index(blocked, &(&1 == false))

      if idx == nil do
        length(x)
      else
        idx + 1
      end
    end)
    |> Enum.product()
  end

  def part_two(data) do
    search(data, fn curr, h, v -> scenic(h, curr) * scenic(v, curr) end)
    |> List.flatten()
    |> Enum.max()
  end

  def main do
    data = input("input.txt")
    IO.puts("Part one: #{part_one(data)}")
    IO.puts("Part two: #{part_two(data)}")
  end
end

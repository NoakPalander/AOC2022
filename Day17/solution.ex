defmodule Solution do
  @rocks [
    [{0, 2}, {0, 3}, {0, 4}, {0, 5}],
    [{0, 3}, {1, 3}, {2, 3}, {1, 2}, {1, 4}],
    [{0, 2}, {0, 3}, {0, 4}, {1, 4}, {2, 4}],
    [{0, 2}, {1, 2}, {2, 2}, {3, 2}],
    [{0, 2}, {1, 2}, {0, 3}, {1, 3}]
  ]

  def input(filename) do
    filename
    |> File.read!()
    |> String.slice(0..-2)
    |> String.to_charlist()
  end

  @spec free?(list(), MapSet.t()) :: boolean()
  def free?(rock, pile) do
    MapSet.intersection(MapSet.new(rock), pile) |> MapSet.size() == 0
  end

  @spec put_all(list(), MapSet.t()) :: MapSet.t()
  def put_all(rock, pile), do: Enum.reduce(rock, pile, fn c, acc -> MapSet.put(acc, c) end)

  @spec push_rock(list(), list(), MapSet.t()) :: list()
  def push_rock(?>, rock, pile) do
    new_rock = Enum.map(rock, fn {r, c} -> {r, c + 1} end)
    {_, max} = Enum.max_by(rock, &elem(&1, 1))

    if max + 1 <= 6 and free?(new_rock, pile), do: new_rock, else: rock
  end

  @spec push_rock(list(), list(), MapSet.t()) :: list()
  def push_rock(?<, rock, pile) do
    new_rock = Enum.map(rock, fn {r, c} -> {r, c - 1} end)
    {_, min} = Enum.min_by(rock, &elem(&1, 1))

    if min - 1 >= 0 and free?(new_rock, pile), do: new_rock, else: rock
  end

  @spec fall(list(), MapSet.t()) :: list()
  def fall(rock, pile) do
    new_rock = Enum.map(rock, fn {r, c} -> {r - 1, c} end)
    {min, _} = Enum.min_by(new_rock, &elem(&1, 0))

    if min >= 0 and free?(new_rock, pile), do: new_rock, else: nil
  end

  @spec move(list(), list(), list(), MapSet.t()) :: {list(), MapSet.t()}
  def move([], jets, rock, pile), do: move(jets, jets, rock, pile)

  @spec move(list(), list(), list(), MapSet.t()) :: {list(), MapSet.t()}
  def move([dir | rest], jets, rock, pile) do
    r = push_rock(dir, rock, pile)

    case next = fall(r, pile) do
      nil -> {rest, put_all(r, pile)}
      _ -> move(rest, jets, next, pile)
    end
  end

  def height(data, count) do
    initial = move(data, data, Enum.map(hd(@rocks), fn {_r, c} -> {3, c} end), MapSet.new())

    @rocks
    |> Stream.cycle()
    |> Stream.drop(1)
    |> Stream.with_index(1)
    |> Enum.reduce_while(initial, fn {rock, idx}, {dirs, pile} ->
      if rem(idx, 1000) == 0 do
        IO.puts("#{idx} : #{MapSet.size(pile)}")
      end

      dbg(dirs)

      if idx == count do
        {:halt, 1 + (pile |> Enum.map(&elem(&1, 0)) |> Enum.max())}
      else
        {top, _} = MapSet.to_list(pile) |> Enum.max_by(&elem(&1, 0))

        adjusted = Enum.map(rock, fn {r, c} -> {r + top + 4, c} end)
        {:cont, move(dirs, data, adjusted, pile)}
      end
    end)
  end

  def part_one(data), do: height(data, 2022)

  def main do
    data = input("test.txt")
    IO.puts "Part one: #{part_one(data)}"
  end
end

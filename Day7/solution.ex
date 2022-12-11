defmodule Solution do
  def input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> dir()
    |> Enum.map(fn {folder, files} -> {folder, files |> Map.values() |> Enum.sum()} end)
    |> folder_sizes()
  end

  defp folder_sizes(folders) do
    folders
    |> Enum.map(fn {folder, total} ->
      sub_size =
        folders
        |> Enum.filter(&String.ends_with?(elem(&1, 0), folder))
        |> Enum.filter(&(elem(&1, 0) != folder))
        |> Map.new()
        |> Map.values()
        |> Enum.sum()

      sub_size + total
    end)
  end

  def dir(commands) do
    dir(commands, %{}, [])
  end

  def dir([], structure, _), do: structure

  def dir(["$ cd .." | rest], structure, [_ | previous_directories]) do
    dir(rest, structure, previous_directories)
  end

  def dir(["$ cd " <> directory | rest], structure, directories) do
    dir(rest, structure, [directory | directories])
  end

  def dir(["$ ls" | rest], structure, dirs) do
    dir(rest, Map.put_new(structure, Enum.join(dirs, "-"), %{}), dirs)
  end

  def dir(["dir " <> _ | rest], structure, directories) do
    dir(rest, structure, directories)
  end

  def dir([file | rest], structure, dirs) do
    [size, name] = String.split(file, " ")

    dir(
      rest,
      Map.update!(structure, Enum.join(dirs, "-"), &Map.put(&1, name, String.to_integer(size))),
      dirs
    )
  end

  def part_one(data) do
    data
    |> Enum.filter(&(&1 <= 100_000))
    |> Enum.sum()
  end

  def part_two(data) do
    sorted = Enum.sort(data, :desc)

    to_remove = abs(70_000_000 - hd(sorted) - 30_000_000)
    Enum.filter(sorted, &(&1 >= to_remove)) |> Enum.min()
  end

  def main do
    data = input("input.txt")
    IO.puts("Part one: #{part_one(data)}")
    IO.puts("Part one: #{part_two(data)}")
  end
end

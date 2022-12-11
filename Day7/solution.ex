defmodule Solution do
  def main do
    data = input("input.txt")
    IO.puts "Part one: #{part_one(data)}"
    IO.puts "Part one: #{part_two(data)}"
  end

  def input(filename) do
      File.read!(filename)
      |> String.split("\n", trim: true)
      |> dir()
      |> Enum.map(fn {folder, files} -> {folder, files |> Map.values() |> Enum.sum()} end)
      |> folder_sizes()
  end

  defp folder_sizes(folder_structure),
    do:
      folder_structure
      |> Enum.map(fn {folder, total_size} ->
        subfolder_size =
          folder_structure
          |> Enum.filter(&String.ends_with?(elem(&1, 0), folder))
          |> Enum.filter(&(elem(&1, 0) != folder))
          |> Map.new()
          |> Map.values()
          |> Enum.sum()

        subfolder_size + total_size
      end)

  defp build_breadcrumbs(directories), do: directories |> Enum.join("-")

  defp dir(commands), do: dir(commands, %{}, [])

  defp dir([], structure, _), do: structure

  defp dir(["$ cd .." | rest], structure, [_ | previous_directories]),
    do: dir(rest, structure, previous_directories)

  defp dir(["$ cd " <> directory | rest], structure, directories),
    do: dir(rest, structure, [directory | directories])

  defp dir(["$ ls" | rest], structure, directories),
    do:
      dir(
        rest,
        structure |> Map.put_new(build_breadcrumbs(directories), %{}),
        directories
      )

  defp dir(["dir " <> _ | rest], structure, directories),
    do: dir(rest, structure, directories)

  defp dir([file | rest], structure, directories) do
    [file_size, file_name] = file |> String.split(" ")

    dir(
      rest,
      structure
      |> Map.update!(build_breadcrumbs(directories), fn old ->
        old |> Map.put(file_name, file_size |> String.to_integer())
      end),
      directories
    )
  end

  def part_one(data) do
    data
    |> Enum.filter(&(&1 <= 100_000))
    |> Enum.sum()
  end

  def part_two(data) do
    sorted_sizes = data |> Enum.sort(:desc)
    root_size = sorted_sizes |> Enum.at(0)
    to_remove = abs(70_000_000 - root_size - 30_000_000)
    sorted_sizes |> Enum.filter(&(&1 >= to_remove)) |> Enum.min()
  end
end

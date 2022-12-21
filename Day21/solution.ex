defmodule Solution do
  def parse(line) do
    [name, op] = String.split(line, ": ")

    action =
      with {n, _} <- Integer.parse(op) do
        n
      else
        _ -> Regex.scan(~r/([a-z]{4,})|(\+|-|\*|\/)/, op) |> Enum.map(&hd/1)
      end

    {name, action}
  end

  def input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse/1)
  end

  def operation(first, second, op) when is_binary(first) or is_binary(second) do
    case op do
      "=" -> "#{first} = #{second}"
      _ -> "(#{first} #{op} #{second})"
    end
  end

  def operation(first, second, "+"), do: first + second
  def operation(first, second, "-"), do: first - second
  def operation(first, second, "*"), do: first * second
  def operation(first, second, "/"), do: div(first, second)

  def find_monkey(name, monkeys), do: Enum.find(monkeys, fn {n, _} -> name == n end)

  def yell({_, op}, _) when is_integer(op), do: op
  def yell({_, "x"}, _), do: "x"

  def yell({_, [first, op, second]}, monkeys) do
    f = find_monkey(first, monkeys) |> yell(monkeys)
    s = find_monkey(second, monkeys) |> yell(monkeys)
    operation(f, s, op)
  end

  def part_one(monkeys) do
    root = find_monkey("root", monkeys)
    yell(root, monkeys) |> IO.puts()
  end

  def part_two(monkeys) do
    {name, [l, _, r]} = find_monkey("root", monkeys)

    m =
      monkeys
      |> List.replace_at(Enum.find_index(monkeys, fn {n, _} -> n == "humn" end), {"humn", "x"})

    yell({name, [l, "=", r]}, m)
  end

  def main do
    monkeys = input("input.txt")
    IO.puts "Part one: #{part_one(monkeys)}"
    IO.puts "Part two: #{part_two(monkeys)}" # solved with a calculator, or with your imagination
  end
end

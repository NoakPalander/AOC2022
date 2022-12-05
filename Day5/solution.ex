defmodule Solution do
  @stacks %{
    1 => 'PDQRVBHF',
    2 => 'VWQZDL',
    3 => 'CPRGQZLH',
    4 => 'BVJFHDR',
    5 => 'CLWZ',
    6 => 'MVGTNPRJ',
    7 => 'SBMVLRJ',
    8 => 'JPD',
    9 => 'VWNCD'
  }

  def parse_instruction(instruction) do
    Regex.scan(~r/move (.*) from (.*) to (.*)/, instruction, capture: :all_but_first)
    |> List.first()
    |> Enum.map(&String.to_integer/1)
  end

  def input(filename) do
    [_, instructions] = File.read!(filename) |> String.split("\n\n")

    instructions
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_instruction/1)
  end

  def pop_count(stack, count), do: {Enum.take(stack, count), Enum.drop(stack, count)}

  def move_crates([], stack, _), do: Map.values(stack) |> Enum.map(&List.first/1) |> List.to_string()
  def move_crates([[count, from, to] | rest], stack, order) do
    {deleted, new} = pop_count(stack[from], count)

    new_stack = Map.replace!(stack, from, new) |> Map.replace(to, order.(deleted) ++ stack[to])
    move_crates(rest, new_stack, order)
  end

  def part_one(instructions), do: move_crates(instructions, @stacks, &Enum.reverse/1)
  def part_two(instructions), do: move_crates(instructions, @stacks, &Quark.id/1)

  def main do
    instructions = input("input.txt")
    IO.puts("Part one: #{part_one(instructions)}")
    IO.puts("Part two: #{part_two(instructions)}")
  end
end

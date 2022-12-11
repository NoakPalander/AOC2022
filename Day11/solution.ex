defmodule Monkey do
  defstruct items: [], count: 0, worry: nil, test: nil, truthy: nil, falsy: nil

  defp calc_worry(monkey, old) do
    case monkey.worry do
      ["*", :old] -> old * old
      ["+", :old] -> old + old
      ["*", n] -> old * n
      ["+", n] -> old + n
    end
  end

  defp update_items(monkey, idx, fun) do
    Map.update!(monkey, :items, fn items ->
      item = Enum.at(items, idx)
      List.replace_at(items, idx, fun.(item))
    end)
  end

  defp delete_at(
         %Monkey{items: items, count: count, worry: op, test: test, truthy: t, falsy: f} = monkey,
         idx
       ) do
    item = Enum.at(items, idx)
    new_items = List.delete_at(monkey.items, idx)

    {item, %Monkey{items: new_items, count: count, worry: op, test: test, truthy: t, falsy: f}}
  end

  defp inspect_item(monkey, idx, worry) do
    monkey
    |> update_items(idx, fn old -> rem(calc_worry(monkey, old), worry) end)
    |> Map.update!(:count, &(&1 + 1))
  end

  defp bored(monkey, idx, adjust) do
    adjusted = if adjust == true, do: update_items(monkey, idx, &div(&1, 3)), else: monkey

    if rem(Enum.at(adjusted.items, idx), adjusted.test) == 0 do
      {adjusted, adjusted.truthy}
    else
      {adjusted, adjusted.falsy}
    end
  end

  def play(monkeys, monkey_id, adjust, worry) do
    {bored_monkey, next_id} = bored(inspect_item(monkeys[monkey_id], 0, worry), 0, adjust)
    {item, monkey} = delete_at(bored_monkey, 0)

    monkeys
    |> Map.replace!(monkey_id, monkey)
    |> Map.update!(next_id, fn x ->
      Map.update!(x, :items, fn items ->
        items ++ [item]
      end)
    end)
  end
end

defmodule Solution do
  def parse_monkey([monkey, items, operation, test, truthy, falsy]) do
    [[id]] = Regex.scan(~r/Monkey ([0-9]+):/, monkey, capture: :all_but_first)
    [[items]] = Regex.scan(~r/Starting items: (.*)/, items, capture: :all_but_first)

    [[operator, operand]] =
      Regex.scan(~r/Operation: new = old (.*) (.*)/, operation, capture: :all_but_first)

    op =
      with {int, _} <- Integer.parse(operand) do
        int
      else
        _ -> :old
      end

    [[condition]] = Regex.scan(~r/Test: divisible by ([0-9]+)/, test, capture: :all_but_first)

    [[true_id]] =
      Regex.scan(~r/If true: throw to monkey ([0-9]+)/, truthy, capture: :all_but_first)

    [[false_id]] =
      Regex.scan(~r/If false: throw to monkey ([0-9]+)/, falsy, capture: :all_but_first)

    {String.to_integer(id),
     %Monkey{
       items: String.split(items, ", ") |> Enum.map(&String.to_integer/1),
       worry: [operator, op],
       test: String.to_integer(condition),
       truthy: String.to_integer(true_id),
       falsy: String.to_integer(false_id)
     }}
  end

  def input(filename) do
    File.read!(filename)
    |> String.split("\n\n")
    |> Enum.map(fn str ->
      str
      |> String.split("\n", trim: true)
      |> Enum.map(&String.trim_leading/1)
      |> parse_monkey()
    end)
    |> Map.new()
  end

  def turn(monkeys, monkey_id, adjust, worry) do
    if monkeys[monkey_id].items == [] do
      monkeys
    else
      turn(Monkey.play(monkeys, monkey_id, adjust, worry), monkey_id, adjust, worry)
    end
  end

  def business(monkeys, _, 0, _, _) do
    monkeys
    |> Map.values()
    |> Enum.map(&Map.get(&1, :count))
    |> Enum.sort(:desc)
    |> Enum.take(2)
    |> Enum.product()
  end

  def business(monkeys, idx, count, adjust, worry) do
    next = turn(monkeys, idx, adjust, worry)

    if idx + 1 >= length(Map.keys(monkeys)) do
      business(next, 0, count - 1, adjust, worry)
    else
      business(next, idx + 1, count, adjust, worry)
    end
  end

  def part_one(monkeys, worry), do: business(monkeys, 0, 20, true, worry)

  def part_two(monkeys, worry), do: business(monkeys, 0, 10_000, false, worry)

  def main do
    monkeys = input("input.txt")
    worry = monkeys
    |> Map.values()
    |> Enum.map(&(&1.test))
    |> Enum.product()

    IO.puts "Part one: #{part_one(monkeys, worry)}"
    IO.puts "Part one: #{part_two(monkeys, worry)}"
  end
end

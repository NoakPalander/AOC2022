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

  defp inspect_item(monkey, worry) do
    Map.update!(
      monkey,
      :items,
      &List.replace_at(&1, 0, rem(calc_worry(monkey, hd(&1)), worry))
    )
    |> Map.update!(:count, &(&1 + 1))
  end

  defp bored(monkey, adjust) do
    adjusted =
      if adjust == true do
        Map.update!(monkey, :items, &List.replace_at(&1, 0, div(hd(&1), 3)))
      else
        monkey
      end

    if rem(hd(adjusted.items), adjusted.test) == 0 do
      {adjusted, adjusted.truthy}
    else
      {adjusted, adjusted.falsy}
    end
  end

  def play(monkeys, monkey_id, adjust, worry) do
    {bored_monkey, next_id} = bored(inspect_item(monkeys[monkey_id], worry), adjust)
    monkey = Map.replace!(bored_monkey, :items, tl(bored_monkey.items))

    monkeys
    |> Map.replace!(monkey_id, monkey)
    |> Map.update!(next_id, fn next ->
      Map.update!(next, :items, &(&1 ++ [hd(bored_monkey.items)]))
    end)
  end
end

defmodule Solution do
  def parse_monkey([
        "Monkey " <> <<id::binary-size(1), _>>,
        "Starting items: " <> items,
        "Operation: new = old " <> <<operator::binary-size(1), _, operand::binary>>,
        "Test: divisible by " <> test,
        "If true: throw to monkey " <> true_id,
        "If false: throw to monkey " <> false_id
      ]) do
    arg =
      with {int, _} <- Integer.parse(operand) do
        int
      else
        _ -> :old
      end

    {String.to_integer(id),
     %Monkey{
       items: String.split(items, ", ") |> Enum.map(&String.to_integer/1),
       worry: [operator, arg],
       test: String.to_integer(test),
       truthy: String.to_integer(true_id),
       falsy: String.to_integer(false_id)
     }}
  end

  def input(filename) do
    File.read!(filename)
    |> String.split("\n\n")
    |> Enum.map(fn str ->
      String.split(str, "\n", trim: true)
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

  def business(monkeys, rounds, adjust, worry) do
    Enum.reduce(0..(rounds * map_size(monkeys) - 1), monkeys, fn idx, ms ->
      turn(ms, rem(idx, map_size(monkeys)), adjust, worry)
    end)
    |> Map.values()
    |> Enum.map(&Map.get(&1, :count))
    |> Enum.sort(:desc)
    |> Enum.take(2)
    |> Enum.product()
  end

  def part_one(monkeys, worry), do: business(monkeys, 20, true, worry)
  def part_two(monkeys, worry), do: business(monkeys, 10_000, false, worry)

  def main do
    monkeys = input("input.txt")
    worry =
      monkeys
      |> Map.values()
      |> Enum.map(& &1.test)
      |> Enum.product()

    IO.puts("Part one: #{part_one(monkeys, worry)}")
    IO.puts("Part two: #{part_two(monkeys, worry)}")
  end
end

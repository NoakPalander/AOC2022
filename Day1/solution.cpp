#include <algorithm>
#include <charconv>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <numeric>
#include <ranges>
#include <string_view>
#include <vector>

namespace fs = std::filesystem;
using namespace std::string_view_literals;

std::vector<std::vector<int>> get_elves(fs::path const& path) {
    std::ifstream reader(path);
    std::string lines(std::istreambuf_iterator<char>(reader), std::istreambuf_iterator<char>{});

    std::vector<std::vector<int>> elves;
    for (auto const line : std::views::split(lines, "\n\n"sv)) {
        auto const groups = std::string_view(line.begin(), line.end());
        std::vector<int> current;

        for (auto const snacks : std::views::split(groups, "\n"sv)) {
            int snack{};
            if (auto [_, ec] = std::from_chars(snacks.data(), snacks.data() + snacks.size(), snack); ec == std::errc()) {
                current.emplace_back(snack);
            }
        }

        elves.emplace_back(current);
        current.clear();
    }

    return elves;
}

std::vector<int> summed(std::vector<std::vector<int>> const& elves) {
    std::vector<int> out;
    std::ranges::transform(elves, std::back_inserter(out), [](auto&& i){
        return std::accumulate(i.begin(), i.end(), 0);
    });

    return out;
}

int part_one(std::vector<std::vector<int>> const& elves) {
    auto const s = summed(elves);
    return *std::ranges::max_element(s);
}

int part_two(std::vector<std::vector<int>> const& elves) {
    auto s = summed(elves);
    std::ranges::sort(s, std::greater<>());
    return std::accumulate(s.begin(), s.begin() + 3, 0);
}

int main() {
    auto elves = get_elves(fs::current_path() / "input.txt");
    std::cout << "Part one: " << part_one(elves) << '\n';
    std::cout << "Part two: " << part_two(elves) << '\n';
}

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <ranges>
#include <vector>
#include <span>
#include <cstdio>
#include <iostream>

namespace stdfs = std::filesystem;
namespace stdr = std::ranges;
using range_t = std::ranges::iota_view<int, int>;

int input(stdfs::path const& path, int(*part)(range_t, range_t, std::span<int>)) {
    int total = 0;
    std::ifstream reader(path);

    for (std::string line; std::getline(reader, line);) {
        int a, b, c, d;
        std::sscanf(line.c_str(), "%d-%d,%d-%d", &a, &b, &c, &d);

        auto task1 = std::views::iota(a, b + 1);
        auto task2 = std::views::iota(c, d + 1);

        std::vector<int> intersection;
        stdr::set_intersection(task1, task2, std::back_inserter(intersection));
        total += part(task1, task2, intersection);
    }

    return total;
}

int part_one(range_t task1, range_t task2, std::span<int> intersection) {
    return stdr::equal(task1, intersection) || stdr::equal(task2, intersection);
}

int part_two([[maybe_unused]] range_t task1, [[maybe_unused]] range_t task2, std::span<int> intersection) {
    return !intersection.empty();
}

int main() {
    auto path = stdfs::current_path().parent_path() / "input.txt";
    std::cout << "Part one: " << input(path, part_one) << '\n';
    std::cout << "Part two: " << input(path, part_two) << '\n';
}
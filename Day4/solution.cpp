#include <algorithm>
#include <filesystem>
#include <fstream>
#include <ranges>
#include <string_view>
#include <vector>
#include <iostream>

namespace stdfs = std::filesystem;
namespace stdr = std::ranges;

std::vector<int> range(std::string_view segment) {
    auto sep = segment.begin() + segment.find('-');
    std::string_view begin(segment.begin(), sep);
    std::string_view end(sep + 1, segment.end());

    int first = std::stoi(begin.data());
    int second = std::stoi(end.data());

    auto it = std::views::iota(first, second + 1);
    return std::vector<int>(it.begin(), it.end());
}

int input(stdfs::path const& path, int(*part)(std::span<int>, std::span<int>, std::span<int>)) {
    int total = 0;
    std::ifstream reader(path);

    for (std::string line; std::getline(reader, line);) {
        auto div = line.begin() + line.find(',');

        std::vector<int> task1 = range({line.begin(), div});
        std::vector<int> task2 = range({div + 1, line.end()});

        std::vector<int> intersection;
        stdr::set_intersection(task1, task2, std::back_inserter(intersection));
        total += part(task1, task2, intersection);
    }

    return total;
}

int part_one(std::span<int> task1, std::span<int> task2, std::span<int> intersection) {
    return stdr::equal(task1, intersection) || stdr::equal(task2, intersection);
}

int part_two([[maybe_unused]] std::span<int> task1, [[maybe_unused]]  std::span<int> task2, std::span<int> intersection) {
    return !intersection.empty();
}

int main() {
    auto path = stdfs::current_path().parent_path() / "input.txt";
    std::cout << "Part one: " << input(path, part_one) << '\n';
    std::cout << "Part two: " << input(path, part_two) << '\n';
}
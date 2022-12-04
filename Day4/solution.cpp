#include <algorithm>
#include <filesystem>
#include <fstream>
#include <ranges>
#include <string_view>
#include <vector>
#include <iostream>

namespace stdfs = std::filesystem;

std::vector<int> range(std::string_view segment) {
    auto sep = segment.begin() + segment.find('-');
    std::string_view begin(segment.begin(), sep);
    std::string_view end(sep + 1, segment.end());

    int first = std::stoi(begin.data());
    int second = std::stoi(end.data());

    auto it = std::views::iota(first, second + 1);
    return std::vector<int>(it.begin(), it.end());
}

int part_one(stdfs::path const& path) {
    int total = 0;
    std::ifstream reader(path);

    for (std::string line; std::getline(reader, line);) {
        auto div = line.begin() + line.find(',');

        std::vector<int> task1 = range({line.begin(), div});
        std::vector<int> task2 = range({div + 1, line.end()});

        std::vector<int> intersection;
        std::ranges::set_intersection(task1, task2, std::back_inserter(intersection));
        if (intersection == task1 || intersection == task2)
            ++total;
    }

    return total;
}

int part_two(stdfs::path const& path) {
    int total = 0;
    std::ifstream reader(path);

    for (std::string line; std::getline(reader, line);) {
        auto div = line.begin() + line.find(',');

        std::vector<int> task1 = range({line.begin(), div});
        std::vector<int> task2 = range({div + 1, line.end()});

        std::vector<int> intersection;
        std::ranges::set_intersection(task1, task2, std::back_inserter(intersection));
        if (!intersection.empty())
            ++total;
    }

    return total;
}

int main() {
    auto path = stdfs::current_path().parent_path() / "input.txt";
    std::cout << "Part one: " << part_one(path) << '\n';
    std::cout << "Part two: " << part_two(path) << '\n';
}
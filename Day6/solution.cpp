#include <algorithm>
#include <filesystem>
#include <fmt/format.h>
#include <fstream>
#include <ranges>
#include <string_view>
#include <unordered_set>

namespace stdfs = std::filesystem;
namespace stdr = std::ranges;

using namespace std::string_view_literals;

std::string input(stdfs::path const& path) {
    std::ifstream reader(path);
    return {std::istreambuf_iterator<char>(reader), std::istreambuf_iterator<char>()};
}

int look(std::string_view data, std::size_t count) {
    for (std::size_t i = 0; i < data.size(); ++i)
        if (std::unordered_set(data.begin() + i, data.begin() + i + count).size() == count)
            return i + count;

    return -1;
}

int main() {
    auto data = input(stdfs::current_path().parent_path() / "input.txt");
    fmt::print("Part one: {}\n", look(data, 4));
    fmt::print("Part two: {}\n", look(data, 14));
}
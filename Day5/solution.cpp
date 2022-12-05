#include <array>
#include <filesystem>
#include <vector>
#include <fstream>
#include <fmt/format.h>
#include <ranges>
#include <string_view>
#include <algorithm>
#include <scn/scn.h>

namespace stdfs = std::filesystem;
namespace stdr = std::ranges;

using namespace std::string_view_literals;

std::string input(stdfs::path const& path) {
    std::ifstream reader(path);
    std::string text{std::istreambuf_iterator<char>(reader), std::istreambuf_iterator<char>()};

    return text.substr(text.find("\n\n") + 2);
}

std::string move_crates(std::array<std::string, 9> crates, std::string_view instructions, bool reverse = true) {
    for (auto line : instructions | std::views::split("\n"sv)) {
        int count, from, to;
        std::ignore = scn::scan(line, "move {} from {} to {}"sv, count, from, to);

        auto x = crates[from - 1].substr(0, count);
        if (reverse)
            stdr::reverse(x);

        crates[to - 1] = x + crates[to - 1];
        crates[from - 1].erase(0, count);
    }

    auto it = std::views::transform(crates, [](auto&& s) { return s[0]; });
    return std::string(it.begin(), it.end());
}

int main() {
    std::array<std::string, 9> crates {
        "PDQRVBHF",
        "VWQZDL",
        "CPRGQZLH",
        "BVJFHDR",
        "CLWZ",
        "MVGTNPRJ",
        "SBMVLRJ",
        "JPD",
        "VWNCD"
    };

    auto instr = input(stdfs::current_path().parent_path() / "input.txt");
    fmt::print("Part one: {}\n", move_crates(crates, instr));
    fmt::print("Part two: {}\n", move_crates(crates, instr, false));
}
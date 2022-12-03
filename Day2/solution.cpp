#include <fstream>
#include <filesystem>
#include <iostream>

namespace stdfs = std::filesystem;

int part_one(stdfs::path const& path) {
    std::ifstream reader(path);
    int sum = 0;

    for (std::string line; std::getline(reader, line);) {
        char opponent = line[0];
        char self = line[2];

        // Won
        if ((self - opponent) % 3 == 0) { // self
            sum += 6;
        }
        // Draw
        else if (opponent == (self - 23)) {
            sum += 3;
        }

        // Rock = 1, Paper = 2, Scissors = 3
        sum += self - 87; // this effectively maps X = 1, Y = 2, Z = 3
    }

    return sum;
}

int part_two(stdfs::path const& path) {
    std::ifstream reader(path);
    int sum = 0;

    for (std::string line; std::getline(reader, line);) {
        char opponent = line[0] - 'A' + 1;
        char self = line[2];

        switch (self) {
            // Need a loss
            case 'X': {
                int loss = (opponent - 1) % 4;
                sum += loss == 0 ? 3 : loss; // handles the wrap around properly instead of going to 0
                break;
            }

            // Need a draw
            case 'Y': {
                sum += opponent + 3;
                break;
            }

            // Need a win
            case 'Z': {
                sum += std::max(1, (opponent + 1) % 4) + 6;
                break;
            }

            default:
                break;
        }
    }

    return sum;
}

int main() {
    auto filepath = stdfs::current_path().parent_path() / "input.txt";
    std::cout << "Part one: " << part_one(filepath) << '\n';
    std::cout << "Part two: " << part_two(filepath) << '\n';
}
import java.io.File
import kotlin.math.max

enum class Weapon(val points: Int) {
    Rock(1),
    Paper(2),
    Scissor(3);

    companion object {
        fun fromInt(points: Int) = Weapon.values().first { it.points == points }
        fun fromKey(key: Char) = when (key) {
            'A', 'X' -> Rock
            'B', 'Y' -> Paper
            'C', 'Z' -> Scissor
            else -> null
        }
    }

    fun loses() = Weapon.fromInt(max(1, (points + 1) % 4))
    fun beats() = ((points - 1) % 4).takeUnless { it == 0 }?.let { Weapon.fromInt(it) } ?: Scissor
}

fun input(file: File) = file.readText().trim().split("\n").map {
    it.mapNotNull(Weapon::fromKey)
}

fun partOne(data: List<List<Weapon>>) = data.sumOf { (opponent, self) ->
    // Loss
    if (self.loses() == opponent) {
        self.points
    }
    // Draw
    else if (self == opponent) {
        self.points + 3
    }
    // Win
    else {
        self.points + 6
    }
}

fun partTwo(data: List<List<Weapon>>) = data.sumOf { (opponent, self) ->
    when (self) {
        // Lose
        Weapon.Rock -> opponent.beats().points
        // Draw
        Weapon.Paper -> opponent.points + 3
        // Win
        else -> opponent.loses().points + 6
    }
}

fun main() {
    val data = input(File("/home/vidar/Dev/AOC2022/Day2/input.txt"))
    println("Part one: ${partOne(data)}")
    println("Part two: ${partTwo(data)}")
}
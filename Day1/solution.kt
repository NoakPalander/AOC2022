import java.io.File

fun input(file: File) = file.readText().split("\n\n").map {
    it.trim().split("\n").map(String::toInt)
}

fun partOne(data: List<List<Int>>) = data.maxOfOrNull(List<Int>::sum)
fun partTwo(data: List<List<Int>>) = data.map(List<Int>::sum).sortedDescending().take(3).sum()

fun main() {
    val data = input(File("input.txt"))
    println("Part one: ${partOne(data)}")
    println("Part two: ${partTwo(data)}")
}
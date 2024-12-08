//> using scala 3.5.2
//> using toolkit 0.6.0
import scala.util.matching.Regex
import scala.compiletime.ops.string

@main
def hello(path: String): Unit =
  val lines =
    io.Source.fromFile(path).getLines.map(line => line.toList).toList

  val xmasRegex: Regex = raw"XMAS".r

  val result = variants(lines)
    .map(variant =>
      variant.map(line => xmasRegex.findAllMatchIn(line.mkString).length).sum
    )
    .sum

  println(result)

def variants(lines: List[List[Char]]): List[List[List[Char]]] =
  val columns = lines.transpose
  val diagonalsFrontSlash = toDiagonals(lines)
  val diagonalsBackSlash = toDiagonals(lines.reverse)
  return lines :: lines
    .map(l => l.reverse) :: columns :: columns.map(l =>
    l.reverse
  ) :: diagonalsFrontSlash :: diagonalsFrontSlash.map(l =>
    l.reverse
  ) :: diagonalsBackSlash :: diagonalsBackSlash.map(l => l.reverse) :: Nil

def toDiagonals(lines: List[List[Char]]): List[List[Char]] =
  val result =
    (for idx <- 0 until lines.length * 2 - 1
    yield (for y <- Math
        .max(idx - (lines.length - 1), 0) to Math.min(idx, lines.length - 1)
    yield lines(y)(idx - y)))
// val result =
//   (for
//     idx <- 0 until lines.length
//     y <- 0 until idx
//   yield lines(y)(idx - y))
  return result.toList.map(line => line.toList)

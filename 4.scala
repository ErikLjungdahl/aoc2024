//> using scala 3.5.2
//> using toolkit 0.6.0
import scala.util.matching.Regex
import scala.compiletime.ops.string
import scala.compiletime.ops.boolean

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

  val result2 = boxes(lines).filter(checkBox).length

  println(result2)

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
  return result.toList.map(line => line.toList)

def boxes(lines: List[List[Char]]): List[List[Char]] =
  val result =
    for
      y <- 0 to lines.length - 3
      x <- 0 to lines(0).length - 3
    yield (
      for
        yi <- y until y + 3
        xi <- x until x + 3
      yield lines(yi)(xi)
    )
  return result.toList.map(line => line.toList)

def checkBox(box: List[Char]): Boolean =
  box match
    case List('M', _, 'S', _, 'A', _, 'M', _, 'S') => true
    case List('M', _, 'M', _, 'A', _, 'S', _, 'S') => true
    case List('S', _, 'S', _, 'A', _, 'M', _, 'M') => true
    case List('S', _, 'M', _, 'A', _, 'S', _, 'M') => true
    case _                                         => false

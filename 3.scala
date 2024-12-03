//> using scala 3.5.2
//> using toolkit 0.6.0
import scala.util.matching.Regex
import scala.compiletime.ops.string

@main
def hello(path: String): Unit =
  val lines = io.Source.fromFile(path).mkString

  val result = multiplyAndSum(lines)

  println(result)

  val result2 = multiplyAndSum(
    raw"do\(\).*?don't\(\)".r
      .findAllMatchIn("do()" ++ lines.replace("\n", "") ++ "don't()")
      .mkString
  )

  println(result2)

def multiplyAndSum(input: String): Number = raw"mul\(\d+,\d+\)".r
  .findAllMatchIn(input)
  .map(mul =>
    mul.toString().drop(4).split(",") match
      case Array(fst, snd) => fst.toInt * snd.init.toInt
  )
  .sum

//> using scala 3.5.2
//> using toolkit 0.6.0
import scala.compiletime.ops.boolean

@main
def hello(path: String): Unit =
  val lines = io.Source.fromFile(path).getLines

  val input = lines
    .map(line => line.split(" ").map((l) => l.toInt).toList)
    .toList

  val result = input.filter(IsOrdered).filter(isSafe).length

  println(result)

def IsOrdered(xs: List[Int]): Boolean =
  xs.sorted == xs | xs.sorted.reverse == xs

def isSafe(xs: List[Int]): Boolean = xs match
  case List()  => true
  case List(x) => true
  case x :: y :: zs =>
    if (1 <= (x - y).abs & (x - y).abs <= 3) isSafe(y :: zs) else false

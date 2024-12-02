//> using scala 3.5.2
//> using toolkit 0.6.0
import scala.compiletime.ops.boolean

@main
def hello(path: String): Unit =
  val lines = io.Source.fromFile(path).getLines

  val input = lines
    .map(line => line.split(" ").map((l) => l.toInt).toList)
    .toList

  val result = input.filter(isOrdered).filter(isSafe)

  println(result.length)

  val result2 = input
    .filterNot(result.contains)
    .map(variants)
    .filter(report =>
      report
        .filter(isOrdered)
        .filter(isSafe)
        .length > 0
    )

  println(result.length + result2.length)

def variants(xs: List[Int]): List[List[Int]] = xs match
  case Nil     => List(List())
  case x :: xs => xs :: (variants(xs).map(ys => x :: ys))

def isOrdered(xs: List[Int]): Boolean =
  xs.sorted == xs | xs.sorted.reverse == xs

def isSafe(xs: List[Int]): Boolean = xs match
  case List()  => true
  case List(x) => true
  case x :: y :: zs =>
    if (1 <= (x - y).abs & (x - y).abs <= 3) isSafe(y :: zs) else false

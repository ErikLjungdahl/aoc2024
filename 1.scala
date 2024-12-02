//> using scala 3.5.2
//> using toolkit 0.6.0

@main
def hello(path: String): Unit =
  val lines = io.Source.fromFile(path).getLines

  val (left, right) = lines
    .map(line =>
      line.split("   ") match
        case Array(fst, snd) => (fst.toInt, snd.toInt)
    )
    .toList
    .unzip

  val result = left.sorted.zip(right.sorted).map(diff).sum

  println(result)

def diff(a: Int, b: Int): Int = (a - b).abs

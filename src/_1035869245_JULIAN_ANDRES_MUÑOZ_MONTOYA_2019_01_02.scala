import scala.util.Random.{nextInt => rand}
import scala.io.StdIn.{readLine => readln}
import scala.annotation.tailrec

object Exam extends App {

  val rows = readln("Number of rows: ").toInt
  val cols = readln("Number of columns: ").toInt

  val matrix = Matrix(rows, cols) {
    (_,_) => rand(10)
  }

  val transposed = Matrix(cols, rows) { (i, j) => matrix(j)(i) }

  def total(m: List[List[Int]]): List[Int] = for (col <- m) yield col.sum

  val totals = total(transposed)

  val eAvg = {
    val evens = totals.zipWithIndex.filter(_._2 % 2 == 0).map(_._1.toDouble)
    val sum = evens.sum
    sum / evens.length
  }

  val solutions = totals.zipWithIndex.filter(_._1 > eAvg).map(e => (s"col ${e._2}", e._1))

  println("Random Matrix: ")
  Printer printMatrix matrix

  Printer separate 1

  println("Totals: ")
  Printer printArray totals

  Printer separate 1

  println("Solutions (starting from col 0): ")
  Printer printArray solutions
  Printer separate 1
  println(s"Number of columns which total is more than $eAvg: ${solutions.length}")
}

// Matrix with a list of columns
object Matrix {
  def apply(rowCount: Int, colCount: Int)(f: (Int, Int) => Int) = (
    for (i <- 0 to rowCount-1) yield (
      for(j <- 0 to colCount-1) yield f(i, j)
    ).toList
  ).toList
}

object Printer {
  def printMatrix(m: List[List[Int]]): Unit = {
    for (row <- m) println(
      s"[${row.mkString.replaceAll(".(?=.)", "$0, ")}]"
    )
  }

  def printArray[T](v: List[T]) = {

    @tailrec
    def loop(v: List[T], acc: String): String = {
      v match {
        case x::Nil => loop(Nil, acc + s"${x.toString}]")
        case x::xs => loop(xs, acc + s"${x.toString}, ")
        case Nil => acc
      }
    }

    println(loop(v, "["))
  }

  def separate(n: Int) = {

    @tailrec
    def loop(i: Int, acc: String): String =
      if (i <= 0) acc
      else loop(i-1, acc + "\n")

    print(loop(n, ""))
  }
}

package rostelecomm

object sudoku {
  def main(args: Array[String]): Unit = {
    val sudoku = Array[Array[Int]](
      Array(2, 9, 1, 5, 4, 3, 6, 7, 8),
      Array(5, 4, 3, 8, 7, 6, 1, 9, 2),
      Array(8, 7, 6, 2, 9, 1, 3, 4, 5),
      Array(4, 3, 2, 7, 6, 5, 8, 1, 9),
      Array(7, 6, 5, 9, 1, 8, 2, 3, 4),
      Array(9, 1, 8, 4, 3, 2, 5, 6, 7),
      Array(3, 2, 9, 6, 5, 4, 7, 8, 1),
      Array(6, 5, 4, 1, 8, 7, 9, 2, 3),
      Array(1, 8, 7, 3, 2, 9, 4, 5, 6))

    printSudoku(sudoku)

    println(if (isCorrect(sudoku)) "корректно"
    else "некорректно")
  }

  def printSudoku(sudoku: Array[Array[Int]]): Unit = {
    for (horizontal <- sudoku) {
      for (each <- horizontal) {
        print(each + " ")
      }
      println()
    }
  }

  def isCorrect(sudoku: Array[Array[Int]]): Boolean = {

    if (sudoku.length != 9) return false
    else for (i <- 0 until 9) {
      if (sudoku(i).length != 9) return false
    }

    val horizontal :scala.collection.mutable.Set[Int] = scala.collection.mutable.Set()
    for (i <- 0 until 9) {
      for (j <- 0 until 9) {
        if (sudoku(i)(j) < 1 || sudoku(i)(j) > 9) return false
        else horizontal addOne (sudoku(i)(j))
      }
      if (horizontal.size != 9) return false
      horizontal.clear()
    }

    val vertical :scala.collection.mutable.Set[Int] = scala.collection.mutable.Set()
    for (i <- 0 until 9) {
      for (j <- 0 until 9) {
        vertical addOne (sudoku(j)(i))
      }
      if (vertical.size != 9) return false
      vertical.clear()
    }

    val segmentSet :scala.collection.mutable.Set[Int] = scala.collection.mutable.Set()
    var i = 0
    while (i < 9) {
      var j = 0
      while (j < 9) {
        for (l <- i until i + 3) {
          for (k <- j until j + 3) {
            segmentSet.add(sudoku(l)(k))
          }
        }
        if (segmentSet.size != 9) return false
        segmentSet.clear()
        j += 3
      }
      i += 3
    }
    true
  }
}
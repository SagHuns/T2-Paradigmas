package Kojun


object Kojun extends App {
  val tabuleiroValores =    Array(Array(2,0,0,0,1,0),
                                  Array(0,0,0,3,0,0),
                                  Array(0,3,0,0,5,3),
                                  Array(0,0,0,0,0,0),
                                  Array(0,0,3,0,4,2),
                                  Array(0,0,0,0,0,0))

  val tabuleiroRegioes = Array(Array(1,1,2,2,2,3),
                                Array(4,4,4,4,4,3),
                                Array(5,6,6,6,4,7),
                                Array(5,5,5,6,7,7),
                                Array(8,8,10,0,0,0),
                                Array(9,9,10,10,0,0))

  val isValid: Array[Array[Int]] => Array[Array[Int]] => Int => Int => Int => Boolean =
    (valuesBoard: Array[Array[Int]]) => (regionsBoard: Array[Array[Int]]) => (row: Int) => (column: Int) => (k: Int) => {
      // Getting the region that the we want to change the value
      val region: Int = regionsBoard(row)(column)

      val validRegion: List[Int] = (for {
        i <- valuesBoard.indices
        // The if is responsible to filter the values so yield gets only the values that passed this if expression
        j <- valuesBoard(i).indices if regionsBoard(i)(j) == region
      } yield valuesBoard(i)(j)).toList

      val neighbours: List[(Int, Int)] = {
        List((row + 1, column), (row - 1, column),
          (row, column + 1), (row, column - 1))
      }

      val validNeighbours: List[Int] = {
        neighbours.filter { case (i, j) =>
          i >= 0 && i < valuesBoard.length &&
            j >= 0 && j < valuesBoard(i).length
        }
      }.map { case (i, j) => valuesBoard(i)(j) }

      val upNeighbour: Boolean = row == (valuesBoard.length - 1) || regionsBoard(row + 1)(column) != region ||
        valuesBoard(row + 1)(column) < k

      val downNeighbour: Boolean = row == 0 || regionsBoard(row - 1)(column) != region ||
        valuesBoard(row - 1)(column) > k

      val lesserRegion: Boolean = k <= validRegion.length

      !validRegion.contains(k) && !validNeighbours.contains(k) && upNeighbour && downNeighbour && lesserRegion
    }


  private def solve(valuesBoard: Array[Array[Int]], regionsBoard: Array[Array[Int]], row: Int, column: Int):
  Option[Array[Array[Int]]] = {
    if (row == valuesBoard.length) Some(valuesBoard)
    else if (column == valuesBoard(row).length) solve(valuesBoard, regionsBoard, row + 1, 0)
    else if (valuesBoard(row)(column) != 0) solve(valuesBoard, regionsBoard, row, column + 1)
    else {
      val kValues: List[Int] = (for {
        k <- 1 to valuesBoard.length
      } yield k).toList
      tryPlacements(valuesBoard, regionsBoard, row, column, kValues)
    }
  }

  def tryPlacements(valuesBoard: Array[Array[Int]], regionsBoard: Array[Array[Int]], row: Int, column: Int, ks: List[Int]):
  Option[Array[Array[Int]]] = {
    val newBoard = valuesBoard.map(_.clone)
    if (ks.isEmpty) None
    else {
      if (isValid(valuesBoard)(regionsBoard)(row)(column)(ks.head)) {
        newBoard(row)(column) = ks.head
        solve(newBoard, regionsBoard, row, column + 1) match {
          case Some(solution) => Some(solution)
          case None => tryPlacements(valuesBoard, regionsBoard, row, column, ks.tail)
        }
      }
      else {
        tryPlacements(valuesBoard, regionsBoard, row, column, ks.tail)
      }
    }
  }

  def printBoard(board: Array[Array[Int]]): Unit = {
    for (row <- board) {
      for (cell <- row) {
        print(cell + " ")
      }
      println()
    }
  }


  solve(tabuleiroValores, tabuleiroRegioes, 0, 0) match {
    case Some(solvedBoard) =>
      println("Solution found:")
      printBoard(solvedBoard)
    case None =>
      println("No solution found.")
  }
}

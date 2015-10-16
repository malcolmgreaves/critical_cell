package criticalcell

object TableExtraction {

  type FindCriticalCell = Table => Option[Content]
  type Table = Seq[Content]

  sealed trait Content {
    def rawRow: Int
    def rawCol: Int
  }
  object Content {
    val toName: Content => String = {
      case Str(content, _, _) => content
      case Dbl(content, _, _) => content.toString
      case Empty(_, _)        => ""
    }
  }

  case class Str(content: String, rawRow: Int, rawCol: Int) extends Content
  case class Dbl(content: Double, rawRow: Int, rawCol: Int) extends Content
  case class Empty(rawRow: Int, rawCol: Int) extends Content
  object Empty {
    val zero = Empty(0, 0)
  }

  case class Start(row: Int, col: Int)
  object Start {
    val default = Start(row = 0, col = 0)
  }

  type IsRow = Boolean

  val numberOfCells: Table => IsRow => Start => Int =
    table => isRow => start =>
      if (isRow)
        table.foldLeft(0) {
          (sameRowOrCol, cell) =>
            if (cell.rawRow == start.row && cell.rawCol >= start.col)
              sameRowOrCol + 1
            else
              sameRowOrCol
        }

      else
        table.foldLeft(0) {
          (sameRowOrCol, cell) =>
            if (cell.rawRow >= start.row && cell.rawCol == start.col)
              sameRowOrCol + 1
            else
              sameRowOrCol
        }

  case class Max(row: Int, col: Int)
  object Max {
    val default = Max(row = 3, col = 3)
  }

  case class Conf(start: Start, max: Max)
  object Conf {
    // In the event that we fail to get the correct critical cell,
    // we default to assuming it exists at (3,3).
    val default = Conf(start = Start.default, max = Max.default)
  }

  /**
   * Assumption:
   *  -- The table is at least a 3x3
   *  -- The critical cell exists within the first 3 rows and 3 olumns.
   */
  lazy val findCritical: FindCriticalCell =
    findCriticalUsing(Conf.default)

  /**
   * Using the configuration, produce a function capable of finding a table's
   * critical cell. This function should evaluate to Some if the input table
   * is well-formed and None otherwise.
   */
  val findCriticalUsing: Conf => FindCriticalCell =
    conf => table => {

        def findIndex(
          numCells: Start => Int,
          max:      Int,
          s:        Start,
          update:   (Start, Int) => Start
        ): Int =
          (0 to max)
            .find { index =>
              val nCurr = numCells(update(s, index))
              val nBefore = numCells(update(s, index - 1))
              nCurr == nBefore
            }
            .getOrElse(max)

      // find the 1st row, starting from row 1,
      // s.t. the # of cells in the row with column # >= 1
      // is equivalent to the # of cells in the previous row
      // (with the same column constraint)
      //
      // so, for instance, we're never going to consider any
      // cells that exist to the left of the first column
      // when doing this counting
      val rowIndexP =
        findIndex(
          numberOfCells(table)(true),
          conf.max.row,
          conf.start,
          (s: Start, newRow: Int) => s.copy(row = newRow)
        )

      // ditto for column
      // We use the found row index (rowIndexP) as the new starting row.
      val colIndexP =
        findIndex(
          numberOfCells(table)(false),
          conf.max.col,
          conf.start
            .copy(row = rowIndexP),
          (s: Start, newCol: Int) => s.copy(col = newCol)
        )

      table
        // locate the critical cell using these found (row,col) coordinates
        // if no such cell matches, then we evaluate to None
        .find { cell =>
          cell.rawRow == rowIndexP && cell.rawCol == colIndexP
        }
    }
}
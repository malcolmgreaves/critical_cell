package criticalcell

object TableExtraction {

  /**
   * Function type for finding the critical cell, if it exists, within a table.
   */
  type FindCriticalCell = Table => Option[Cell]

  /*
   * A table is a sequence of cells. These cells do not need to be in-order.
   * The table also need not be well-formed.
   */
  type Table = Seq[Cell]

  /**
   * The abstract data type for a table cell.
   * Every cell has its row and column indicies.
   */
  sealed trait Cell {
    def rawRow: Int
    def rawCol: Int
  }
  object Cell {
    /** Converts a cell into a string representation. */
    val toName: Cell => String = {
      case Str(content, _, _) => content
      case Dbl(content, _, _) => content.toString
      case Empty(_, _)        => ""
    }
  }

  /** Cell implementation when its content is a string. */
  case class Str(content: String, rawRow: Int, rawCol: Int) extends Cell

  /** Cell implementation when its content is a real-valued number. */
  case class Dbl(content: Double, rawRow: Int, rawCol: Int) extends Cell

  /** Cell instance for something that contains no content. */
  case class Empty(rawRow: Int, rawCol: Int) extends Cell
  object Empty {
    /** An empty cell with row and column indicies equal to 0. */
    val zero = Empty(0, 0)
  }

  /** Algorithm configuration: what row and column to start search. */
  case class Start(row: Int, col: Int)
  object Start {
    /** The default searching position is from (0, 0). */
    val default = Start(row = 0, col = 0)
  }

  /** Type documentation for number of cells function. */
  type IsRow = Boolean

  /**
   * Function for finding the number of cells in the table on a particular axis.
   * numberOfCells iterates through the table, looking at cells that are in a
   * particular row (or column if IsRow=false). The evaluated Int is the number
   * of cells found in the table's row (or column).
   */
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

  /** Algorithm configuration: upper bound on row and column during search. */
  case class Max(row: Int, col: Int)
  object Max {
    /** Default maximum row and column to consider is (3,3). */
    val default = Max(row = 3, col = 3)
  }

  /** Algorithm configuration: the Start and Max instances for search. */
  case class Conf(start: Start, max: Max)
  object Conf {
    /** Default configuration uses the defaults from Start and Max. */
    val default = Conf(start = Start.default, max = Max.default)
  }

  /** FindCriticalCell function using the default configuration. */
  lazy val findCritical: FindCriticalCell =
    // Note the "lazy val" because we use "findCriticalUsing" before it is
    // defined. The lazy modifier delays initialization until absolutely needed.
    findCriticalUsing(Conf.default)

  /**
   * Using the configuration, produce a function capable of finding a table's
   * critical cell. This function should evaluate to Some if the input table
   * is well-formed and None otherwise.
   */
  val findCriticalUsing: Conf => FindCriticalCell =
    conf => table => {

        /** Method for finding the critical cell inidices. */
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
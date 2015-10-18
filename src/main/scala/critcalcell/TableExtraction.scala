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
    def row: Int
    def col: Int
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
  case class Str(content: String, row: Int, col: Int) extends Cell

  /** Cell implementation when its content is a real-valued number. */
  case class Dbl(content: Double, row: Int, col: Int) extends Cell

  /** Cell instance for something that contains no content. */
  case class Empty(row: Int, col: Int) extends Cell
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

  /** Type documentation for numberOfCells function. */
  type IsRow = Boolean

  /**
   * Function for finding the number of cells in the table on a given axis.
   * numberOfCells iterates through the table, looking at cells that are in a
   * particular row (or column if IsRow=false). The evaluated Int is the number
   * of cells found in the table's row (or column).
   */
  val numberOfCells: Table => IsRow => Start => Int =
    table => isRow => start => {

      val shouldIncrement: Cell => Boolean =
        if (isRow)
          // cell in same row, any column right of start
          cell =>
            cell.row == start.row && cell.col >= start.col
        else
          // cell in same column, any row above start
          cell =>
            cell.col == start.col && cell.row >= start.row

      table
        .foldLeft(0) {
          (trace, cell) =>
            if (shouldIncrement(cell))
              trace + 1
            else
              trace
        }
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
    // defined.
    // The lazy modifier delays initialization until absolutely needed.
    findCriticalUsing(Conf.default)

  /**
   * Using the configuration, produce a function capable of finding a table's
   * critical cell. This function should evaluate to Some if the input table
   * is well-formed and None otherwise.
   */
  val findCriticalUsing: Conf => FindCriticalCell =
    conf => table => {

        // Helper method for finding the critical cell inidices.
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

      // Find 1st row such that the # of cells in the row
      // is equivalent to the # of cells in the previous row.
      val rowIndexP =
        findIndex(
          numberOfCells(table)(true),
          conf.max.row,
          conf.start,
          (s: Start, newRow: Int) => s.copy(row = newRow)
        )

      // Find 1st column such that the # of cells in the column
      // is equivalent to the # of cells in the previous column.
      val colIndexP =
        findIndex(
          numberOfCells(table)(false),
          conf.max.col,
          // We use the found row index (rowIndexP) as the new starting row.
          conf.start.copy(row = rowIndexP),
          (s: Start, newCol: Int) => s.copy(col = newCol)
        )

      table
        // Locate the critical cell using these found (row,col) coordinates.
        // If no such cell matches, then the table has no critical cell.
        // Indicated by evaluating to None here.
        .find { cell =>
          cell.row == rowIndexP && cell.col == colIndexP
        }
    }
}
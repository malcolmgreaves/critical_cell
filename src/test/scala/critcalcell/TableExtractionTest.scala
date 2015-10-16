package criticalcell

import org.scalatest.FunSuite

class TableExtractionTest extends FunSuite {

  // Bring in the critical cell finding algorithm & types.
  import TableExtraction._

  //
  // The data used in the tests.
  //

  val criticalCell: Cell =
    Str("Yes", 1, 1)

  val expectedCriticalCell: Option[Cell] =
    Some(criticalCell)

  val tWeatherDays: Table =
    Seq(
      Empty.zero,
      // column headers
      Str("Raining?", 0, 1),
      Str("Hot?", 0, 2),
      Str("Cloudy?", 0, 2),
      // row headers
      Str("Monday", 1, 0),
      Str("Tuesday", 2, 0),
      Str("Wednesday", 3, 0),
      // data cells
      criticalCell,
      Str("No", 1, 2),
      Str("No", 1, 3),
      Str("Yes", 2, 1),
      Str("No", 2, 2),
      Str("Yes", 2, 3),
      Str("No", 3, 1),
      Str("Yes", 3, 2),
      Str("No", 3, 3)
    )

  val notWellFormedTable: Table =
    Seq(
      // headers
      // Note now their locations in the table are staggered.
      Str("TopRowCol", 0, 2),
      Str("MiddleRowCol", 1, 1),
      Str("BottomRowCol", 2, 0),
      // data cells
      // These also follow the star-stepping stagger of the headers.
      Dbl(1.1, 1, 2),
      Dbl(2.2, 2, 1),
      Dbl(3.3, 2, 2)
    )

  // 
  // Functions to assist in testing.
  //

  /**
   * mkShuffle accepts a seed, creates a pseudo-random number generator with it,
   * and returns a function that is able to shuffle the order of Cells within a
   * given Table instance.
   */
  val mkShuffle: Long => Table => Table =
    seed => {
      val r = new scala.util.Random(seed)
      table =>
        table
          .map { cell => (cell, r.nextInt) }
          .sortBy { case (_, rando) => rando }
          .map { case (cell, _) => cell }
    }

  //
  // Tests !
  //

  test("Critical cell found in table") {
    assert(findCritical(tWeatherDays) == expectedCriticalCell)
  }

  test("Critical found in shuffled table") {
    val shuffle = mkShuffle(System.currentTimeMillis)
    for (i <- 0 until 20) {
      val s = shuffle(tWeatherDays)
      assert(findCritical(s) == expectedCriticalCell)
    }
  }

  test("Did not find critical cell in not well-formed table") {
    assert(findCritical(notWellFormedTable) == None)
  }

  test("Did not find critical in shuffled non WFT") {
    val shuffle = mkShuffle(System.currentTimeMillis)
    for (i <- 0 until 20) {
      val s = shuffle(notWellFormedTable)
      assert(findCritical(s) == None)
    }
  }

}
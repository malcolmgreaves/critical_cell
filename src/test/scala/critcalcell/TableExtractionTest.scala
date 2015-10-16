package criticalcell

import org.scalatest.FunSuite

class ExampleTables extends FunSuite {

  // Bring in the critical cell finding algorithm & types.
  import TableExtraction._

  val criticalCell: Content =
    Str("Yes", 1, 1)

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

  val expectedCriticalCell: Option[Content] =
    Some(criticalCell)

  test("Critical cell found in table") {
    assert(findCritical(tWeatherDays) == expectedCriticalCell)
  }

  val mkShuffle: Long => Table => Table =
    seed => {
      val r = new scala.util.Random(seed)
      table =>
        table
          .map { cell => (cell, r.nextInt) }
          .sortBy { case (_, rando) => rando }
          .map { case (cell, _) => cell }
    }

  test("Critical found in shuffled table") {
    val shuffle = mkShuffle(System.currentTimeMillis)
    for (i <- 0 until 20) {
      val s = shuffle(tWeatherDays)
      assert(findCritical(s) == expectedCriticalCell)
    }
  }

  val notWellFormedTable: Table =
    Seq(
      Str("TopRowCol", 0, 2),
      Str("MiddleRowCol", 1, 1),
      // row headers
      Str("BottomRowCol", 2, 0),
      // data cells
      Dbl(1.1, 1, 2),
      Dbl(2.2, 2, 1),
      Dbl(3.3, 2, 2)
    )

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

  // test("Did not ")

}
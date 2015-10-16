package criticalcell

import org.scalatest.FunSuite

class ExampleTables extends FunSuite {

  // Bring in the critical cell finding algorithm & types.
  import TableExtraction._

  val criticalCell: Content =
    Str("Yes", 1, 1)

  val tWeatherDays: Table =
    Seq(
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

  test("Found expected critical cell in table") {
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

  val updatedFindCritical: FindCriticalCell =
    findCriticalUsing(Conf.default.copy(start = Start(0, 0)))

  val tWithEmpty: Table =
    tWeatherDays :+ Empty.zero

  test("Critical found in table with empty added") {
    assert(updatedFindCritical(tWithEmpty) == expectedCriticalCell)
  }

  test("Critical found in shuffled table with empty added") {
    val shuffle = mkShuffle(System.currentTimeMillis)
    for (i <- 0 until 20) {
      val s = shuffle(tWithEmpty)
      assert(updatedFindCritical(s) == expectedCriticalCell)
    }
  }

}
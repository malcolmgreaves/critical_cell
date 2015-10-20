critical_cell
=============

[![Build Status](https://travis-ci.org/malcolmgreaves/critical_cell.svg?branch=master)](https://travis-ci.org/malcolmgreaves/critical_cell)

Critical cell finding algorithm using functional programming in Scala.

* Execute `./sbt test` to execute the tests.
* Is this code open source? Yes! You may use this code under the terms of the Apache 2.0 license.

### Example

    import criticalcell.TableExtraction._
    val existingData: Table = ... // unordered sequence of Cell instances
                                  // Simpliest possible table: Seq.empty[Cell]
    val table = existingData :+ Str("a Cell at (0,0) that has string content", 0, 0)
    findCritical(table) match {
      case Some(c) => 
        println(s"Found critical cell in table: c")
      case None =>
        println("No critical cell found in table.")
    }


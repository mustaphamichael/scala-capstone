package observatory

import java.time.LocalDate
import org.junit.Assert._
import org.junit._

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object

  import Extraction._

  val stationCsv =
    """
      |010013,,,
      |724017,03707,+37.358,-078.438
      |724017,,+37.350,-078.433
      |""".stripMargin

  val tempCsv =
    """
      |010013,,11,25,39.2
      |724017,,08,11,81.14
      |724017,03707,12,06,32
      |724017,03707,01,29,35.6
      |""".stripMargin

  @Test def `locateTemperatures in a given station`: Unit = {
    val s = stationCsv.trim.split("\n").toList
    val t = tempCsv.trim.split("\n").toList
    val expected = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.000000000000001)
    )
    assertEquals(expected, locate(2015, s, t).collect.toList)
  }

  @Test def `compute the yearly average temerature`: Unit = {
    val records = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )
    val expected = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    assertEquals(expected, locationYearlyAverageRecords(records).toList)
  }
}

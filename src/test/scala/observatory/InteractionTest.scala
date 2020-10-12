package observatory

import scala.collection.concurrent.TrieMap
import org.junit.Assert._
import org.junit.Test

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  import Visualization._

  @Test def `convert tile to location`: Unit = {
    val t1 = Tile(0, 0, 0)
    val t2 = Tile(0, 1, 0)
    val t3 = Tile(1, 0, 0)
    assertEquals(Location(85.0511287798066, -180), t1.toLocation)
    assertEquals(Location(-85.0511287798066, -180), t2.toLocation)
    assertEquals(Location(85.0511287798066, 180), t3.toLocation)
  }

  @Test def `check color interpolation from tile`: Unit = {
    val loc = Tile(0, 0, 0).toLocation
    val temps = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )

    val colors = Seq(
      (60.0, Color(255, 255, 255)), (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)), (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)), (-60.0, Color(0, 0, 0))
    )

    assertEquals(Color(255, 255, 0), interpolateColor(colors, predictTemperature(temps, loc)))
  }
}

package observatory

import scala.collection.concurrent.TrieMap
import org.junit.Assert._
import org.junit.Test

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  @Test def `Convert tile to location`: Unit = {
    val t1 = Tile(0, 0, 0)
    val t2 = Tile(0, 1, 0)
    val t3 = Tile(1, 0, 0)
    assertEquals(Location(85.0511287798066, -180), t1.toLocation)
    assertEquals(Location(-85.0511287798066, -180), t2.toLocation)
    assertEquals(Location(85.0511287798066, 180), t3.toLocation)
  }
}

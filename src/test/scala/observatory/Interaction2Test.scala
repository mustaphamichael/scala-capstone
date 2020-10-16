package observatory

import org.junit.Assert._
import org.junit.Test

trait Interaction2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive user interface", 6) _

  // Implement tests for methods of the `Interaction2` object

  import Interaction2._

  val layer: Signal[Layer] = Signal(availableLayers.head)

  @Test def `get yearly bounds difference`: Unit = {
    val diff = (1975 to 2015).size
    assertEquals(diff, yearBounds(layer)().size)
  }

  @Test def `select year and keep it within bounds`: Unit = {
    val y1 = 1980
    val y2 = 1974 // lesser
    val y3 = 2016 // greater
    assertEquals(1980, yearSelection(layer, Var(y1))())
    assertEquals(1975, yearSelection(layer, Var(y2))())
    assertEquals(2015, yearSelection(layer, Var(y3))())
  }

  @Test def `generate url pattern`: Unit = {
    val year = Var(2015)
    val expected = "target/temperatures/2015/{z}/{x}-{y}.png"
    assertEquals(expected, layerUrlPattern(layer, year)())
  }

  @Test def `display capture name`: Unit = {
    val year = Var(2015)
    val expected = "Temperatures (2015)"
    assertEquals(expected, caption(layer, year)())
  }

}

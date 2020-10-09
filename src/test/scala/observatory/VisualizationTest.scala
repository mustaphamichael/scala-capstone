package observatory

import org.junit.Assert._
import org.junit.Test

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  // Implement tests for the methods of the `Visualization` object

  import Visualization._

  @Test def `find the shortest distance between two points`: Unit = {
    val p1 = Location(37.0, 119.0)
    val p2 = Location(-37.0, 61.0)
    val p3 = Location(-37.0, 299.0)
    assert(p1.distanceTo(p1) == 0, "Equal points should return zero(0)")
    assert(p1.distanceTo(p2) == math.Pi * 90, "Antipodes should return Pi")
    assert(doubleCheck(p2.distanceTo(p3), 51.885), "Distance using arc formula")
  }

  @Test def `predictTemperature should return 'existing' temperature if there exist a location distance of zero(0)`: Unit = {
    val temps = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    assert(predictTemperature(temps, Location(37.35, -78.433)) == 27.3, "A temperature exists for this location")
  }

  @Test def `predictTemperature should return temperature for a new location`: Unit = {
    val temps = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    assert(doubleCheck(predictTemperature(temps, Location(37.35, -78.432)), 27.06),
      "A new temperature should be returned")
  }

  /**
    * Color Test Cases
    */
  @Test def `color arithmetic should be in the threshold`: Unit = {
    val c60 = Color(255, 255, 255)
    val c32 = Color(255, 0, 0)
    assertEquals(Color(255, 255, 255), c60 + c32)
    assertEquals(Color(0, 255, 255), c60 - c32)
    assertEquals(Color(255, 0, 0), c32 * 2)
  }

  @Test def `linear interpolation should estimate the right color`: Unit = {
    val x0 = 12.0
    val y0 = Color(255, 255, 0)
    val x1 = 32.0
    val y1 = Color(255, 0, 0)
    assertEquals(Color(255, 153, 0), interpolateLinearly(x0, y0, x1, y1, 20))
  }

  @Test def `interpolate color through a spectrum of available colors`: Unit = {
    val rgbs = List((0.0, Color(255, 0, 0)), (1.0, Color(0, 0, 255)))
    assertEquals(Color(255, 0, 0), interpolateColor(rgbs, -0.1)) // less than or equal to least
    assertEquals(Color(0, 0, 255), interpolateColor(rgbs, 2.0)) // greater than or equal to most
    assertEquals(Color(128, 0, 128), interpolateColor(rgbs, 0.5)) // within color range
    assertEquals(Color(191, 0, 64),
      interpolateColor(List((-89.0, Color(255, 0, 0)), (0.0, Color(0, 0, 255))), -66.75)) // within color range
  }

}

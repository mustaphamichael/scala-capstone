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

}

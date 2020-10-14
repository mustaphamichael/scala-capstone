package observatory

import org.junit.Test

trait ManipulationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  // Implement tests for methods of the `Manipulation` object

  import Manipulation._

  @Test def `make grid should return a function of GridLocation => Temperature`: Unit = {
    val temps = Seq((Location(37.35, -78.433), 27.3))
    val grid = makeGrid(temps)
    assert(grid(GridLocation(37, -78)) == 27.3)
  }

  @Test def `compute the average temperature`: Unit = {
    val t1 = Seq((Location(10, 10), 10.0), (Location(20, 20), 20.0))
    val t2 = Seq((Location(10, 10), 15.0), (Location(20, 20), 10.0))
    val temps = Seq(t1, t2)
    val avg = average(temps)
    assert(avg(GridLocation(10, 10)) == 12.5) // (25 / 2)
    assert(avg(GridLocation(20, 20)) == 15) // (30 / 2)
  }

  @Test def `check the deviation in temperature`: Unit = {
    val t1 = Seq((Location(10, 10), 10.0), (Location(20, 20), 20.0))
    val normals = makeGrid(t1)
    val temps = Seq((Location(10, 10), 15.0), (Location(20, 20), 20.0))

    val deviatedGrid = deviation(temps, normals)

    assert(deviatedGrid(GridLocation(10, 10)) == 5) // (15 - 10)
    assert(deviatedGrid(GridLocation(20, 20)) == 0) // (20 - 20)
  }

}

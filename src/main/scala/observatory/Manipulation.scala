package observatory

import Visualization._

/**
  * 4th milestone: value-added information
  */

class Grid {
  private val width = 360
  private val height = 180
  private val store = new Array[Temperature](width * height)

  private def position(loc: GridLocation): Int = {
    val x = loc.lon + 180 // (width / 2)
    val y = math.abs(loc.lat - 90) // (height / 2)
    width * y + x
  }

  def populate(temperatures: Iterable[(Location, Temperature)]): Grid = {
    for {
      lon <- -180 until 180
      lat <- 90 until -90 by -1
      loc = GridLocation(lat, lon)
    } store.update(position(loc), predictTemperature(temperatures, loc.location))
    this
  }

  def get(gridLocation: GridLocation): Temperature = store(position(gridLocation))

}


object Manipulation extends ManipulationInterface {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val grid = new Grid().populate(temperatures)
    gridLocation: GridLocation => grid.get(gridLocation)
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    def computeAverage(temps: Iterable[Temperature]): Temperature = temps.sum / temps.size

    // extract the temperature for each grid location
    def gridTemperature(f: GridLocation => Temperature): Iterable[(GridLocation, Temperature)] = for {
      lon <- -180 until 180
      lat <- 90 until -90 by -1
      loc = GridLocation(lat, lon)
    } yield loc -> f(loc)

    val average = temperaturess
      .map(makeGrid) // make a grid for each year
      .flatMap(gridTemperature)
      .groupBy { case (location, _) => location } // group the temperatures by their grid location
      .mapValues(_.map(_._2)) // extract the temperature of each location
      .mapValues(computeAverage) // compute average per grid location

    gridLocation: GridLocation => average(gridLocation)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = new Grid().populate(temperatures)
    gridLocation: GridLocation => grid.get(gridLocation) - normals(gridLocation)
  }

}

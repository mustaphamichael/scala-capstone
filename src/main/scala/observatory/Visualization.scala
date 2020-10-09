package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val power = 2

    def weighingFunction(other: Location): Double = 1 / math.pow(location.distanceTo(other), power)

    // using spatial interpolation
    val locations = temperatures.map(_._1)
    if (locations.forall(loc => loc.distanceTo(location) != 0)) {
      // use interpolating function
      val n = temperatures.map { case (loc, temp) => weighingFunction(loc) * temp -> weighingFunction(loc) }
      val (top, bottom) = n.fold((0.0, 0.0))((acc, elem) => (acc._1 + elem._1) -> (acc._2 + elem._2))
      top / bottom
    } else {
      // return the temperature of already existing location
      temperatures.find(_._1 == location).get._2
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    // sort the points by the proximity of the temperature values
    val temperatures = points.toVector
      .map { p => p._1 - value -> p }
      .sortBy(_._1)

    // Extract temperature
    def getTemperature(v: (Double, Color)): Temperature = v._1

    // Extract color
    def getColor(v: (Double, Color)) = v._2

    val x0 = getTemperature(temperatures(0)._2)
    val y0 = getColor(temperatures(0)._2)
    val x1 = getTemperature(temperatures(1)._2)
    val y1 = getColor(temperatures(1)._2)

    if (value <= x0) y0
    else if (value >= x1) y1
    else interpolateLinearly(x0, y0, x1, y1, value)
  }

  /**
    * See https://en.wikipedia.org/wiki/Linear_interpolation
    *
    * @param x0 Lower bound temperature
    * @param y0 Lower bound color
    * @param x1 Upper bound temperature
    * @param y1 Upper bound color
    * @param x  Temperature value
    * @return
    */
  def interpolateLinearly(x0: Temperature, y0: Color, x1: Temperature, y1: Color, x: Temperature): Color = {
    val div = x1 - x0
    (y0 * ((x1 - x) / div)) + (y1 * ((x - x0) / div))
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180
    val temperatureByLocation = temperatures.groupBy(_._1).mapValues(_.head._2) // Map[Location, Temperature]
    val colorByTemperature = colors.groupBy(_._1).mapValues(_.head._2) // Map[Temperature, Color]

    // map the latitude and longitude to the image pixel position
    val longitudeOnXAxis = ((-180 until 180) zip (0 to width)).toMap
    val latitudeOnYAxis = ((90 until -90 by -1) zip (0 to height)).toMap

    // create pixel
    val pixels: Array[Pixel] = Array(width * height)

    for {
      (loc, temp) <- temperatureByLocation
      x = longitudeOnXAxis(loc.lon.toInt)
      y = latitudeOnYAxis(loc.lat.toInt)
      position = (width * x) + y
      color = colorByTemperature(temp)
    } pixels.update(position, Pixel(color.red, color.green, color.blue, 0))

    // create image
    Image(width, height, pixels)
  }
}


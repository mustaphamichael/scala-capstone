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
    val power = 4.0

    def weighingFunction(other: Location): Double = 1.0 / math.pow(location.distanceTo(other), power)

    // using spatial interpolation
    @scala.annotation.tailrec
    def loop(list: List[(Location, Temperature)], sum: Double, weightSum: Double): Double =
      list match {
        case Nil => sum / weightSum
        case (loc, temp) :: _ if loc.distanceTo(location) < 1 => temp // use the temperature for close locations (< 1km)
        case (loc, temp) :: xs => loop(xs, sum + weighingFunction(loc) * temp, weightSum + weighingFunction(loc))
      }

    loop(temperatures.toList, 0.0, 0.0)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    // sort the points by the proximity of the temperature values
    val temperatures = points.toVector
      .map { p => math.abs(p._1 - value) -> p }
      .sortBy { case (d, tuple) => d }.take(2)
      .sortBy { case (d, tuple) => tuple._1 }

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

    // map the latitude and longitude to the image pixel position [(90, -180) => (0, 0)]
    val longitudeOnXAxis = (-180 until 180) zip (0 until width)
    val latitudeOnYAxis = (90 until -90 by -1) zip (0 until height)

    // create pixel
    val pixels: Array[Pixel] = Array.fill(width * height)(0)

    // update the pixels for the entire map
    for {
      (lon, x) <- longitudeOnXAxis
      (lat, y) <- latitudeOnYAxis
      position = (width * y) + x
      temp = predictTemperature(temperatures, Location(lat, lon))
      color = interpolateColor(colors, temp)
    } pixels.update(position, Pixel(color.red, color.green, color.blue, 0))

    // create image
    Image(width, height, pixels)
  }
}

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
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    ???
  }

}


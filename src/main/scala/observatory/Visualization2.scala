package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(point: CellPoint,
                            d00: Temperature,
                            d01: Temperature,
                            d10: Temperature,
                            d11: Temperature
                           ): Temperature = {
    val x = point.x
    val y = point.y
    val s00 = d00 * (1 - x) * (1 - y)
    val s01 = d01 * (1 - x) * y
    val s10 = d10 * x * (1 - y)
    val s11 = d11 * x * y
    s00 + s01 + s10 + s11
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(grid: GridLocation => Temperature,
                    colors: Iterable[(Temperature, Color)],
                    tile: Tile
                   ): Image = {
    val width = 256
    val height = 256
    val zoomLevel = 8 // (256 = 2⁸)

    val pixels = new Array[Pixel](width * height)

    // Parallel data structure used for faster processing
    val axes = (for (x <- 0 until width; y <- 0 until height) yield (x, y)).par

    val n = math.pow(2, zoomLevel).toInt
    val x0 = tile.x * n
    val y0 = tile.y * n
    val z0 = tile.zoom + zoomLevel
    for {
      (x, y) <- axes
      position = (width * y) + x
      subTile = Tile(x + x0, y + y0, z0)
      temp = predictTemperature(subTile, grid)
      color = Visualization.interpolateColor(colors, temp)
    } pixels.update(position, Pixel(color.red, color.green, color.blue, 127))

    Image(width, height, pixels)
  }

  // Predict the temperature of the square tile
  private def predictTemperature(tile: Tile, f: GridLocation => Temperature): Temperature = {
    // convert tile to location
    val location = tile.toLocation // the tile's location on the map
    val lon = location.lon.round.toInt // lower bound of longitude
    val lat = location.lat.round.toInt // lower bound of latitude

    // Since the interpolation is Unit Square, the difference between the lower and upper bound is 1
    val lonUp = lon + 1
    val latUp = lat + 1

    // use bilinear interpolation to predict temperature
    val d00 = f(GridLocation(lat, lon))
    val d01 = f(GridLocation(latUp, lon))
    val d10 = f(GridLocation(lat, lonUp))
    val d11 = f(GridLocation(latUp, lonUp))

    // x & y will always be positive
    val x = location.lon - lon
    val y = location.lat - lat

    bilinearInterpolation(CellPoint(x, y), d00, d01, d10, d11)
  }

}

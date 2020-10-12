package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import math.pow
import Visualization._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = tile.toLocation

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  // Hint taken from https://www.coursera.org/learn/scala-capstone/discussions/weeks/3/threads/GREm0Rr6EeepgQ59jal4IA/replies/XZ9A-Br_EeepgQ59jal4IA/comments/x0EicRx3EeejzQokTe8yRg
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val width = 256
    val height = 256
    val zoom = 8 // (256 = 2⁸)

    // Parallel data structure used for faster processing
    val pixels: Array[Pixel] = Array.fill(width * height)(0)

    // Each pixel in a tile can be thought of as a sub-tile at a higher zoom level (256 = 2⁸).
    // See http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Subtiles for computing subtiles

    val x0 = (pow(2, zoom) * tile.x).toInt
    val y0 = (pow(2, zoom) * tile.y).toInt
    val z0 = tile.zoom + zoom
    for {
      x <- 0 until width
      y <- 0 until height
      position = (width * y) + x
      subTile = Tile(x + x0, y + y0, z0)
      temp = predictTemperature(temperatures, tileLocation(subTile))
      color = interpolateColor(colors, temp)
    } pixels.updated(position, Pixel(color.red, color.green, color.blue, 127))

    Image(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } generateImage(year, Tile(x, y, zoom), data)
  }
}

package observatory

import scala.math.{abs, acos, cos, round, sin, toRadians}

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  /**
    * See https://en.wikipedia.org/wiki/Antipodes#Mathematical_description
    *
    * @return True if two points are Antipodes
    */
  private def areAntipodes(other: Location): Boolean =
    this.lat + other.lat == 0 &&
      (this.lon + other.lon == 180 || abs(this.lon - other.lon) == 180)

  /**
    * See https://en.wikipedia.org/wiki/Great-circle_distance
    *
    * @param other Other location
    * @return The shortest distance between the two locations
    */
  def distanceTo(other: Location): Double = {
    val radius = 6371.0 // the earth's radius
    val centralAngle = if (this == other) 0
    else if (areAntipodes(other)) math.Pi
    else {
      val absLongitudeDiff = toRadians(abs(this.lon - other.lon)) // Absolute longitude difference
      acos(sin(toRadians(this.lat)) * sin(toRadians(other.lat)) +
        cos(toRadians(this.lat)) * cos(toRadians(other.lat)) * cos(absLongitudeDiff))
    }
    centralAngle * radius
  }
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  *
  * @param x    X coordinate of the tile
  * @param y    Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int)

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  *
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  *
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  *
  * @param red   Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue  Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int) {
  // Trim the color to match the threshold (0 ≤ int ≤ 255)
  private def trim: Color = {
    def threshold(color: Int): Int = if (color <= 0) 0 else math.min(color, 255)

    Color(threshold(red), threshold(green), threshold(blue))
  }

  def +(other: Color): Color = Color(red + other.red, green + other.green, blue + other.blue).trim

  def -(other: Color): Color = Color(red - other.red, green - other.green, blue - other.blue).trim

  def *(multiplier: Double): Color = Color(round(red * multiplier).toInt, round(green * multiplier).toInt, round(blue * multiplier).toInt).trim
}

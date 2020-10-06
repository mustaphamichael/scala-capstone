package observatory

import java.time.LocalDate

import org.apache.spark.rdd.RDD
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface with Spark {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stations = Source.fromInputStream(getClass.getResourceAsStream(stationsFile))("UTF-8").getLines().toList
    val temperatures = Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile))("UTF-8").getLines().toList
    locate(year, stations, temperatures).collect.toSeq
  }

  case class Station(stn: String, wban: String, location: Option[Location])

  case class Temp(stn: String, wban: String, month: Int, day: Int, temperature: Temperature)

  def locate(year: Year, stations: List[String], temperatures: List[String]): RDD[(LocalDate, Location, Temperature)] = {
    val stationRdd = sc.parallelize(stations).flatMap(parseStation)
    val temperatureRdd = sc.parallelize(temperatures).map(parseTemperature)
    // filter out stations without GPS coordinates
    val filteredStation = stationRdd.filter(_.location.isDefined).map(s => (s.stn, s.wban) -> s)

    // do a inner join on the station and temperature records
    val joined = temperatureRdd
      .map(t => (t.stn, t.wban) -> t)
      .join(filteredStation)

    // construct the output triplet
    joined.map {
      case (_, t) =>
        val temp = t._1
        val station = t._2
        (LocalDate.of(year, temp.month, temp.day), station.location.get, temp.temperature)
    }.persist
  }

  private def parseStation(line: String): Option[Station] = {
    val arr = line.split(",")
    if (arr.length < 4) None
    else
      Some(Station(
        stn = arr(0),
        wban = arr(1),
        location = if (arr(2) == "" || arr(3) == "") None else Some(Location(arr(2).toDouble, arr(3).toDouble))
      ))
  }

  private def parseTemperature(line: String): Temp = {
    val arr = line.split(",")
    Temp(
      stn = arr(0),
      wban = arr(1),
      month = arr(2).toInt,
      day = arr(3).toInt,
      temperature = fahToCel(arr(4).toDouble)
    )
  }

  /**
    * @param temp Temperature in Fahrenheit
    * @return Temperature in Celsius
    */
  private def fahToCel(temp: Temperature): Double = (temp - 32) / 1.8

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val rdd = sc.parallelize(records.toSeq)
    val pair = rdd.map(r => r._2 -> r)
    pair.groupByKey()
      .mapValues(_.map(v => v._3))
      .mapValues(computeAverage)
      .collect
  }

  private def computeAverage(temps: Iterable[Temperature]): Temperature =
    temps.sum / temps.size
}

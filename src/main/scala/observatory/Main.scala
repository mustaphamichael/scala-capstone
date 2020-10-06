package observatory

import org.apache.spark.sql.SparkSession
import org.apache.spark.{SparkConf, SparkContext}

object Main extends App {

  import org.apache.log4j.{Level, Logger}

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  // Extract data


}

trait Spark {
  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")
  @transient lazy val sc: SparkContext = new SparkContext(conf)
//  lazy val spark: SparkSession = SparkSession.builder().config(conf).getOrCreate()
}

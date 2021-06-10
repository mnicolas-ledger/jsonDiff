package example
import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime
import java.io.File
import java.io.PrintWriter
import scala.io.Source
import io.circe.JsonObject
import io.circe.Json
import io.circe.parser._

object Hello extends Greeting with App {
  def sortJson(j: Json): Json =
    j.fold(j, _ => j, _ => j, _ => j, sortArray, sortObject)
  def sortArray(j: Vector[Json]): Json =
    if(j.isEmpty) Json.arr()
    else {
      Json.fromValues {
        if(j.head.isObject) {
          j.map(sortJson).sortBy(c => c.hcursor.downField("id").as[Int].getOrElse(throw new IllegalArgumentException(s"Missing field id in object $c")))
        } else j.sortBy(_.toString())
      }
    }
  def sortObject(j: JsonObject): Json =
    Json.fromJsonObject(j.mapValues(j => sortJson(j)))
  val v3Path = "/Users/mnicolas/Downloads/mcu_versions.json"
  val v3 = parse(Source.fromFile(v3Path).getLines().mkString("\n")).getOrElse(???)
  val v3Sorted = sortJson(v3)
  val v6Path = "/Users/mnicolas/Downloads/mcu_versions_scala.json"
  val v6 = parse(Source.fromFile(v6Path).getLines().mkString("\n")).getOrElse(???)
  val v6Sorted = sortJson(v6)
  v3Sorted == v6Sorted
  val s = parse("\"2020-07-16T11:24:34.71414Z\"").getOrElse(???).as[ZonedDateTime].getOrElse(???)
  DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSX").format(s) == "2020-07-16T11:24:34.714140Z"
  // val name = "/home/krauscher/Downloads/v2"
  // val json = parse(Source.fromFile(name).getLines().mkString("\n")).getOrElse(???)
  val pw = new PrintWriter(new File(s"$v3Path.sorted"))
  pw.write(v3Sorted.noSpacesSortKeys)
  pw.write("\n")
  pw.close()
  val pw2 = new PrintWriter(new File(s"$v6Path.sorted"))
  pw2.write(v6Sorted.noSpacesSortKeys)
  pw2.write("\n")
  pw2.close()
}

trait Greeting {
  lazy val greeting: String = "hello"
}

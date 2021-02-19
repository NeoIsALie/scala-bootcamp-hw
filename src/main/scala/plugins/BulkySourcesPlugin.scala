package plugins

import sbt._
import Keys._

import scala.util.Try

object BulkySourcesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  lazy val bulkySources = taskKey[Seq[(Int, File)]](
    "Return Seq of files with lines of code more than threshold"
  )
  lazy val bulkyThresholdInLines = settingKey[Int](
    "Min bulky threshold lines, files that have more lines is bulky. Default is 100."
  )

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    bulkySources := findBulkySources((Compile / sources).value, bulkyThresholdInLines.value),
    (Test / bulkySources) := findBulkySources((Test / sources).value, bulkyThresholdInLines.value)
  )

  private def findBulkySources(files: Seq[File], threshold: Int) = {
    val bulkyFiles =
      for {
        file <- files
        lines = Try(sbt.IO.readLines(file).size).getOrElse(-1)
        if lines >= threshold
      } yield (lines, file)

    bulkyFiles.sorted.reverse
  }
}
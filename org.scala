#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import java.io.{BufferedWriter, FileWriter}

import scala.util.Try

object CmdLineUtils {
  val Usage =
    """
    |Usage:
    |  org add member [id]
    |  org update member [id] [field] [value]
    |  org update member [id] [field] add|remove [value] #For multi-value fields
    |  org remove member [id]
  """.stripMargin

  def printMessage(message: String): Unit = {
    Console.err.println(message)
    System.exit(1)
  }

  def promptForValue(field: Field): String = {
    import io.StdIn._
    if (field.multiValue)
      readLine(s"Please enter '|' separated values for ${field.name}: ")
    else
      readLine(s"Please enter a value for ${field.name}: ")
  }
}

import CmdLineUtils._

args.toList match {
  case "add" :: "member" :: id :: Nil =>
    Members.add(id)
  case "update" :: "member" :: id :: field :: value :: Nil =>
    Members.update(id, field, value)
  case "update" :: "member" :: id :: field :: action :: value :: Nil =>
    Members.update(id, field, action, value)
  case "remove" :: "member" :: id :: Nil =>
    Members.remove(id)
  case _ =>
    printMessage(Usage)
}

object Members {
  private val CsvFile = "members.csv"

  def add(id: String): Unit = {
    val memberFields = CsvHelper.readHeader(CsvFile)

    val newRow = memberFields.map {
      promptForValue
    }

    CsvHelper.addRow(CsvFile, newRow)
  }

  def update(id: String, field: String, value: String) = {
    println(s"Updating member $id: field $field -> $value")
  }

  def update(id: String, field: String, action: String, value: String) = {
    println(s"Updating member $id: multi-value field $field -> $value")
  }

  def remove(id: String) = {
    println(s"Removing member $id")
  }
}

case class Field(name: String, multiValue: Boolean) {
  override def toString = {
    if (multiValue)
      name + " " + Field.MultiValueIndicator
    else
      name
  }
}

object Field {
  private val MultiValueIndicator = "(m)"

  def apply(value: String): Field = {
    val multiValueIndicatorIndex = value.toLowerCase.indexOf(MultiValueIndicator)

    if (multiValueIndicatorIndex == -1)
      Field(value.trim, multiValue = false)
    else
      Field(value.substring(0, multiValueIndicatorIndex).trim, multiValue = true)
  }
}

type Header = Seq[Field]
type Row = Seq[String]

case class Csv(header: Header, rows: Seq[Row])

object CsvHelper {

  private val Separator = ","

  def addRow(filename: String, newRow: Row): Unit = {
    safelyWrite(filename, append = true, Seq(newRow.mkString(Separator)))
  }

  def write(filename: String, csv: Csv): Unit = {
    val header = csv.header.mkString(Separator)
    val rows = csv.rows.map { _.mkString(Separator) }

    safelyWrite(filename, append = false, header +: rows)
  }

  def readHeader(filename: String): Header = {
    safelyRead(filename) { lines =>
      if (lines.hasNext) {
        parseLine(lines.next()).map { Field.apply }
      } else {
        sys.error(s"File $filename is missing a header")
      }
    }
  }

  def read(filename: String): Csv = {
    safelyRead(filename) { lines =>
      if (lines.hasNext) {
        val header = parseLine(lines.next()).map { Field.apply }
        val entries = lines.map { parseLine }.toSeq
        Csv(header, entries)
      } else {
        sys.error(s"File $filename is missing a header")
      }
    }
  }

  private def safelyRead[T](filename: String)(f: Iterator[String] => T) = {
    val source = io.Source.fromFile(filename)
    val attemptToProcess = Try { f(source.getLines()) }
    source.close()
    attemptToProcess.get
  }

  private def safelyWrite(filename: String, append: Boolean, rows: Seq[String]): Unit = {
    var bw = new BufferedWriter(new FileWriter(filename, append))
    val attempt = Try {
      rows.foreach { row =>
        bw.write(row)
        bw.newLine()
      }
      bw.flush()
    }

    Try { bw.close() }

    attempt.get
  }

  private def parseLine(line: String): Seq[String] = line.split(",").map(_.trim)
}



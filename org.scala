#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import java.io.{BufferedWriter, FileWriter}

import scala.util.Try

Try {
  execute()
}.recover {
  case ex: Exception =>
    Console.err.println(ex.getMessage)
    System.exit(1)
}

def execute(): Unit = {
  args.toList match {
    case "add" :: "member" :: id :: Nil =>
      Members.add(id)
    case "update" :: "member" :: id :: field :: value :: Nil =>
      Members.update(id, field, value)
    case "update" :: "member" :: id :: action :: field :: value :: Nil =>
      Members.update(id, action, field, value)
    case "remove" :: "member" :: id :: Nil =>
      Members.remove(id)
    case _ =>
      sys.error(CmdLineUtils.Usage)
  }
}

object CmdLineUtils {
  val Usage =
    """
      |Usage:
      |  org add member [id]
      |  org update member [id] [field] [value]
      |  org update member [id] add|remove [field] [value] #For multi-value fields
      |  org remove member [id]
    """.stripMargin

  def promptForValue(field: Field): String = {
    import io.StdIn._
    if (field.multiValue)
      readLine(s"Please enter '|' separated values for ${field.name}: ")
    else
      readLine(s"Please enter a value for ${field.name}: ")
  }
}

object Members {
  private val CsvFile = "members.csv"

  def add(id: String): Unit = {
    val memberFields = CsvHelper.readHeader(CsvFile)

    val newRow = memberFields.map { field =>
      if (field.id)
        id
      else
        CmdLineUtils.promptForValue(field)
    }

    CsvHelper.addRow(CsvFile, newRow)
  }

  def update(id: String, field: String, value: String) = {
    println(s"Updating member $id: field $field -> $value")
  }

  def update(id: String, action: String, field: String, value: String) = {
    println(s"Updating member $id: multi-value field $field -> $value")
  }

  def remove(id: String) = {
    println(s"Removing member $id")
  }
}

case class Field(name: String, multiValue: Boolean, id: Boolean) {
  override def toString = {
    if (multiValue)
      name + " " + Field.MultiValueIndicator
    else if (id)
      name + " " + Field.IdIndicator
    else
      name
  }
}

object Field {
  private val MultiValueIndicator = "(m)"
  private val IdIndicator = "(id)"

  def apply(value: String): Field = {
    val multiValueIndicatorIndex = value.toLowerCase.indexOf(MultiValueIndicator)
    val idIndicatorIndex = value.toLowerCase.indexOf(IdIndicator)

    Field(value.split("(").head.trim, multiValue = multiValueIndicatorIndex != -1, id = idIndicatorIndex != -1)
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
    val rows = csv.rows.map {
      _.mkString(Separator)
    }

    safelyWrite(filename, append = false, header +: rows)
  }

  def readHeader(filename: String): Header = {
    safelyRead(filename) { lines =>
      if (lines.hasNext) {
        parseLine(lines.next()).map {
          Field.apply
        }
      } else {
        sys.error(s"File $filename is missing a header")
      }
    }
  }

  def read(filename: String): Csv = {
    safelyRead(filename) { lines =>
      if (lines.hasNext) {
        val header = parseLine(lines.next()).map {
          Field.apply
        }
        val entries = lines.map {
          parseLine
        }.toSeq
        Csv(header, entries)
      } else {
        sys.error(s"File $filename is missing a header")
      }
    }
  }

  private def safelyRead[T](filename: String)(f: Iterator[String] => T) = {
    val source = io.Source.fromFile(filename)
    val attemptToProcess = Try {
      f(source.getLines())
    }
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

    Try {
      bw.close()
    }

    attempt.get
  }

  private def parseLine(line: String): Seq[String] = line.split(",").map(_.trim)
}



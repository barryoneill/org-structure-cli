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

def updatingMembers(f: (Csv, ValidRefs) => Csv): Unit = {
  val membersData = Members.loadData
  val teamsData = Teams.loadData
  val titlesData = Titles.loadData

  val updatedData = f(membersData, ValidRefs(membersData.ids, teamsData.ids, titlesData.ids))

  Members.writeData(updatedData)
}

def withMembers(f: Csv => Unit): Unit = {
  f(Members.loadData)
}

def execute(): Unit = {
  args.toList match {
    case "get" :: "member" :: id :: Nil =>

    case "find" :: "member" :: field :: value :: Nil =>

    case "add" :: "member" :: id :: Nil =>
      updatingMembers { (data, validRefs) =>
        OrgData.add(data, id, validRefs)
      }
    case "update" :: "member" :: id :: field :: Nil =>
      updatingMembers { (data, validRefs) =>
        OrgData.update(data, id, Overwrite, field, validRefs)
      }
    case "update" :: "member" :: id :: action :: field :: Nil =>
      updatingMembers { (data, validRefs) =>
        OrgData.update(data, id, MultiValueFieldAction(action), field, validRefs)
      }
    case "remove" :: "member" :: id :: Nil =>
      updatingMembers { (data, validRefs) =>
        OrgData.remove(data, id)
      }
    case "validate" :: Nil =>
      val membersData = Members.loadData
      val teamsData = Teams.loadData
      val titlesData = Titles.loadData
      val validRefs = ValidRefs(membersData.ids, teamsData.ids, titlesData.ids)

      OrgData.validate(membersData, validRefs)
      OrgData.validate(teamsData, validRefs)
      OrgData.validate(titlesData, validRefs)
    case _ =>
      sys.error(CmdLineUtils.Usage)
  }
}

object CmdLineUtils {
  import io.StdIn._

  val Usage =
    """
      |Usage:
      |  org add member [id]
      |  org update member [id] [field] [value]
      |  org update member [id] add|remove [field] [value] #For multi-value fields
      |  org remove member [id]
      |  org get member [id]
      |  org find member [field] [value]
    """.stripMargin

  private case class IndexedValue(index: Int, value: String)

  def promptForValue(fieldName: String, multiValue: Boolean, validValues: Seq[String] = Seq.empty): String = {
    val prompt =
      if (multiValue)
        s"Please enter '|' separated values for $fieldName: "
      else
        s"Please enter a value for $fieldName: "

    val value = readLine(prompt)

    if (value.contains(",")) sys.error("No commas please!")

    if (validValues.nonEmpty && !validValues.contains(value)) {
      val filteredValidValues = validValues.filter(_.toLowerCase.contains(value.toLowerCase))

      val suggestedValues =
        if (filteredValidValues.nonEmpty) filteredValidValues
        else validValues

      val validValueLookup = suggestedValues.zipWithIndex.map { vv => IndexedValue(vv._2+1, vv._1) }

      val indexPrompt =
        s"""
           |Invalid value. Choose from:
           |  ${validValueLookup.map { v => s"${v.index}: ${v.value}" }.mkString("\n")}
        """.stripMargin

      promptForIndexedValue(indexPrompt, validValueLookup)
    } else {
      value
    }
  }

  private def promptForIndexedValue(indexPrompt: String, validValueLookup: Seq[IndexedValue]): String = {
    val index = readLine(indexPrompt).toInt

    validValueLookup.find { _.index == index }.map(_.value).getOrElse {
      promptForIndexedValue(indexPrompt, validValueLookup)
    }
  }
}


// Org data

trait OrgData {
  def filename: String
  def loadData: Csv = Csv.read(filename)
  def writeData(data: Csv): Unit = Csv.write(filename, data)
}

case class ValidRefs(members: Seq[String], teams: Seq[String], titles: Seq[String]) {
  def forField(field: Field): Seq[String] = {
    if (field.memberRef) members
    else if (field.teamRef) teams
    else if (field.titleRef) titles
    else Seq.empty
  }
}

object Members extends OrgData {
  override val filename = "members.csv"
}

object Teams extends OrgData {
  override val filename = "teams.csv"
}

object Titles extends OrgData {
  override val filename = "titles.csv"
}

object OrgData {
  def add(currentData: Csv, id: String, validRefs: ValidRefs): Csv = {
    val newRow = currentData.header.fields.map { field =>
      if (field.id) id
      else
        CmdLineUtils.promptForValue(field.name, field.multiValue, validRefs.forField(field))
    }

    currentData.addRow(newRow)
  }

  def update(currentData: Csv, id: String, action: FieldAction, fieldName: String, validRefs: ValidRefs): Csv = {
    val field = currentData.header.field(fieldName)
    val value = CmdLineUtils.promptForValue(field.name, field.multiValue, validRefs.forField(field))
    currentData.updateFieldValue(id, action, fieldName, value)
  }

  def remove(currentData: Csv, id: String): Csv = {
    currentData.removeRow(id)
  }

  def validate(data: Csv, validRefs: ValidRefs): Unit = {
    // Check the integrity of references
    for {
      row <- data.rows
      cell <- row.cells
    } {
      require(!cell.field.memberRef || validRefs.members.contains(cell.value), s"Invalid member reference [${cell.value}] in row [${row.index}].")
      require(!cell.field.teamRef || validRefs.teams.contains(cell.value), s"Invalid team reference [${cell.value}] in row [${row.index}].")
      require(!cell.field.titleRef || validRefs.titles.contains(cell.value), s"Invalid title reference [${cell.value}] in row [${row.index}].")
    }
  }
}


// Csv models


sealed trait FieldAction {
  def update(currentValue: String, change: String): String
}

case object Overwrite extends FieldAction {
  override def update(currentValue: String, change: String) = change
}

sealed trait MultiValueFieldAction extends FieldAction {
  val Delimiter = "|"
}

object MultiValueFieldAction {
  def apply(value: String): MultiValueFieldAction = {
    value match {
      case "add" => MultiValueAdd
      case "remove" => MultiValueRemove
      case _ => sys.error(s"Unknown action [$value]")
    }
  }
}

case object MultiValueAdd extends MultiValueFieldAction {
  override def update(currentValue: String, change: String) = {
    (currentValue.split(Delimiter) :+ change).mkString(Delimiter)
  }
}

case object MultiValueRemove extends MultiValueFieldAction {
  override def update(currentValue: String, change: String) = {
    currentValue.split(Delimiter).filterNot(_ == change).mkString(Delimiter)
  }
}

case class Field(name: String, multiValue: Boolean, id: Boolean, memberRef: Boolean, teamRef: Boolean, titleRef: Boolean) {
  override def toString = {
    if (multiValue)
      name + " " + Field.MultiValueIndicator
    else if (id)
      name + " " + Field.IdIndicator
    else if (memberRef)
      name + " " + Field.MemberRefIndicator
    else if (teamRef)
      name + " " + Field.TeamRefIndicator
    else if (titleRef)
      name + " " + Field.TitleRefIndicator
    else
      name
  }
}

object Field {
  private val MultiValueIndicator = "(multi)"
  private val IdIndicator = "(id)"
  private val MemberRefIndicator = "(member)"
  private val TeamRefIndicator = "(team)"
  private val TitleRefIndicator = "(title)"

  def apply(header: String): Field = {
    val multiValueIndicatorIndex = header.toLowerCase.indexOf(MultiValueIndicator)
    val idIndicatorIndex = header.toLowerCase.indexOf(IdIndicator)
    val memberRefIndicatorIndex = header.toLowerCase.indexOf(MemberRefIndicator)
    val teamRefIndicatorIndex = header.toLowerCase.indexOf(TeamRefIndicator)
    val titleRefIndicatorIndex = header.toLowerCase.indexOf(TitleRefIndicator)

    val fieldName = header.split("(").head.trim

    Field(
      name = fieldName,
      multiValue = multiValueIndicatorIndex != -1,
      id = idIndicatorIndex != -1,
      memberRef = memberRefIndicatorIndex != -1,
      teamRef = teamRefIndicatorIndex != -1,
      titleRef = titleRefIndicatorIndex != -1
    )
  }
}

case class Header(fields: Seq[Field]) {
  def field(fieldName: String): Field = {
    fields.find { _.name == fieldName }.getOrElse {
      sys.error(s"No such field [$fieldName]. Choose from [${fields.map(_.name)}]")
    }
  }
}

case class Cell(field: Field, value: String)

case class Row(index: Int, cells: Seq[Cell]) {

  val id = cells.toList.filter(_.field.id) match {
    case singleId :: Nil => singleId.value
    case Nil => sys.error("Row has no id field")
    case multipleIds => sys.error(s"Row has more than one id field [${multipleIds.map(_.field.name).mkString(",")}")
  }

  def cell(fieldName: String): Cell = {
    cells.find(_.field.name == fieldName).getOrElse {
      sys.error(s"Unknown field name [$fieldName]")
    }
  }
}

object Row {
  def apply(header: Header, index: Int, values: Seq[String]): Row = {
    val cells = header.fields.zip(values).map {
      case (field, value) => Cell(field, value)
    }
    Row(index, cells)
  }
}
case class Csv(header: Header, rows: Seq[Row]) {

  val ids: Seq[String] = rows.map(_.id).distinct

  require(ids.size == rows.size, s"Ids are not unique. There are [${ids.size}] ids and [${rows.size}] rows.")

  // Returns the row with the given id
  def row(id: String): Row = {
    rows.find { _.id == id }.getOrElse {
      sys.error(s"Unknown id [$id]")
    }
  }

  // Returns the id of all matching rows
  def findRows(fieldName: String, value: String): Seq[String] = {
    rows.filter { _.cell(fieldName).value.contains(value) }.map(_.id)
  }

  def addRow(newRow: Seq[String]): Csv = {
    copy(rows = rows :+ Row(header, rows.size, newRow))
  }

  def removeRow(id: String): Csv = {
    copy(rows = rows.filterNot(_.id == id))
  }

  def updateFieldValue(rowId: String, action: FieldAction, fieldName: String, change: String): Csv = {
    val oldRow = row(rowId)

    val oldCell = oldRow.cell(fieldName)
    val newValue = action.update(oldCell.value, change)
    val newRowCells = oldRow.cells.patch(oldRow.cells.indexOf(oldCell), Seq(oldCell.copy(value = newValue)), 1)
    val newRow = oldRow.copy(cells = newRowCells)
    val newRows = rows.patch(oldRow.index, Seq(newRow), 1)

    copy(rows = newRows)
  }
}

object Csv {

  private val Separator = ","

  def write(filename: String, csv: Csv): Unit = {
    val header = csv.header.fields.mkString(Separator)
    val rows = csv.rows.map { _.cells.map(_.value).mkString(Separator) }

    safelyWrite(filename, header +: rows)
  }

  def read(filename: String): Csv = {
    safelyRead(filename) { lines =>
      if (lines.hasNext) {
        val headerLine = parseLine(lines.next())
        val headerFields = headerLine.distinct
        require(headerLine.size == headerFields.size, "There are duplicate names in the header.")

        val header = Header(headerFields.map { Field(_) })

        val rows = lines.zipWithIndex.map {
          case (line, index) =>
            val values = parseLine(line)
            require(values.size == headerFields.size, s"Row [$index] does not have the same number of fields as the header.")
            Row(header, index, values)
        }.toSeq
        Csv(header, rows)
      } else {
        sys.error(s"File [$filename] is missing a header")
      }
    }
  }

  // Closes the file after use
  private def safelyRead[T](filename: String)(f: Iterator[String] => T) = {
    val source = io.Source.fromFile(filename)
    val attemptToProcess = Try { f(source.getLines()) }
    source.close()
    attemptToProcess.get
  }

  // Closes the file after use
  private def safelyWrite(filename: String, rows: Seq[String]): Unit = {
    var bw = new BufferedWriter(new FileWriter(filename, false))
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



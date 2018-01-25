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

def execute(): Unit = {
  args.toList match {
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
    data.validate()

    // Check the integrity of references
    data.rows.foreach { row =>
      validateRefs(row, data.header.memberRefFields, validRefs.members)
      validateRefs(row, data.header.teamRefFields, validRefs.teams)
      validateRefs(row, data.header.titleRefFields, validRefs.titles)
    }
  }

  private def validateRefs(row: Row, refFields: Seq[Field], validValues: Seq[String]): Unit = {
    refFields.foreach { refField =>
      val ref = row.value(refField.index)
      require(validValues.contains(ref), s"Invalid ref [$ref] in row [${row.index}]")
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

case class Field(index: Int, name: String, multiValue: Boolean, id: Boolean, memberRef: Boolean, teamRef: Boolean, titleRef: Boolean) {
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

  def apply(index: Int, value: String): Field = {
    val multiValueIndicatorIndex = value.toLowerCase.indexOf(MultiValueIndicator)
    val idIndicatorIndex = value.toLowerCase.indexOf(IdIndicator)
    val memberRefIndicatorIndex = value.toLowerCase.indexOf(MemberRefIndicator)
    val teamRefIndicatorIndex = value.toLowerCase.indexOf(TeamRefIndicator)
    val titleRefIndicatorIndex = value.toLowerCase.indexOf(TitleRefIndicator)

    val name = value.split("(").head.trim

    Field(
      index = index,
      name = name,
      multiValue = multiValueIndicatorIndex != -1,
      id = idIndicatorIndex != -1,
      memberRef = memberRefIndicatorIndex != -1,
      teamRef = teamRefIndicatorIndex != -1,
      titleRef = titleRefIndicatorIndex != -1
    )
  }
}

case class Header(fields: Seq[Field]) {
  // The header should have one and only one id field
  val idField = fields.toList.filter(_.id) match {
    case singleId :: Nil => singleId
    case Nil => sys.error("Header has no id field")
    case multipleIds => sys.error(s"Header has more than one id field [${multipleIds.map(_.name).mkString(",")}")
  }

  val memberRefFields = fields.filter(_.memberRef)
  val teamRefFields = fields.filter(_.teamRef)
  val titleRefFields = fields.filter(_.titleRef)

  def field(fieldName: String): Field = {
    fields.find { _.name == fieldName }.getOrElse {
      sys.error(s"No such field [$fieldName]. Choose from [${fields.map(_.name)}]")
    }
  }
}

case class Row(index: Int, values: Seq[String]) {
  def value(fieldIndex: Int): String = {
    values.apply(fieldIndex)
  }
}

case class Csv(header: Header, rows: Seq[Row]) {

  // Validate the integrity of the data
  def validate(): Unit = {
    // Validate that all rows have the correct number of fields
    rows.foreach { row =>
      require(row.values.size == header.fields.size, s"Row [${row.index}] does not have the same number of fields [${header.fields.size}] as the header")
    }
  }

  val ids: Seq[String] = rows.map { rowId }

  def rowId(row: Row): String = row.value(header.idField.index)

  // Returns the row with the given id
  def row(id: String): Row = {
    rows.find { rowId(_) == id }.getOrElse {
      sys.error(s"Unknown id [$id]")
    }
  }

  // Returns the id of all matching rows
  def findRows(fieldName: String, value: String): Seq[String] = {
    val fieldIndex = header.field(fieldName).index
    rows.filter { _.value(fieldIndex).contains(value) }.map { rowId }
  }

  def addRow(newRow: Seq[String]): Csv = {
    copy(rows = rows :+ Row(rows.size, newRow))
  }

  def removeRow(id: String): Csv = {
    copy(rows = rows.filterNot(rowId(_) == id))
  }

  def updateFieldValue(rowId: String, action: FieldAction, fieldName: String, change: String): Csv = {
    val oldRow = row(rowId)
    val fieldToUpdate = header.field(fieldName)

    val newValue = action.update(oldRow.value(fieldToUpdate.index), change)
    val newRowValues = oldRow.values.patch(fieldToUpdate.index, Seq(newValue), 1)
    val newRow = oldRow.copy(values = newRowValues)
    val newRows = rows.patch(oldRow.index, Seq(newRow), 1)

    copy(rows = newRows)
  }
}

object Csv {

  private val Separator = ","

  def write(filename: String, csv: Csv): Unit = {
    val header = csv.header.fields.mkString(Separator)
    val rows = csv.rows.map { _.values.mkString(Separator) }

    safelyWrite(filename, header +: rows)
  }

  def read(filename: String): Csv = {
    safelyRead(filename) { lines =>
      if (lines.hasNext) {
        val headerFields = parseLine(lines.next()).zipWithIndex.map {
          case (value, index) => Field.apply(index, value)
        }
        val header = Header(headerFields)
        val rows = lines.zipWithIndex.map {
          case (line, index) => Row(index, parseLine(line))
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



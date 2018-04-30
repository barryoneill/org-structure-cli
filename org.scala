#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import java.io.{BufferedWriter, FileWriter}

import scala.sys.process._
import scala.util.Try

val TestMode = sys.env.getOrElse("TEST_MODE", "0") == "1"

val OrgDataDir = sys.env.getOrElse("ORG_DATA_DIR", ".") + "/"

Try {
  execute()
}.recover {
  case ex: Exception =>
    Console.err.println(ex.getMessage)
    System.exit(1)
}

def searchData(data: Csv, search: String): Unit = {
  val results = data.header.fields.flatMap { field =>
    data.findRows(field.name, search)
  }
  Console.println(results.map(_.toJson).mkString("[ ", ", ", " ]"))
}

def findDataById(data: Csv, id: String): Unit = {
  Console.println(data.row(id).toJson)
}

def findDataByField(data: Csv, field: String, value: String): Unit = {
  Console.println(data.findRows(field, value).map(_.toJson).mkString("[ ", ", ", " ]"))
}

def updatingMembers(f: (Csv, ValidRefs) => Csv): Unit = {
  val memberData = Members.loadData
  val teamIds = if (memberData.header.fields.exists(_.isTeamRef)) Teams.loadData.ids else Nil
  val titleIds = if (memberData.header.fields.exists(_.isTitleRef)) Titles.loadData.ids else Nil

  val updatedData = f(memberData, ValidRefs(memberData.ids, teamIds, titleIds))

  Members.writeData(updatedData)

  // If we're not in test mode then commit the change with a consistent message for easy parsing afterwards
  if (!TestMode)
    Seq("sh", "-c", s"""cd $OrgDataDir; git commit -am "${args.mkString(" ")}"; cd - """).!
}

def execute(): Unit = {
  args.toList match {
    case "member" :: search if search.nonEmpty =>
      searchData(Members.loadData, search.mkString(" "))

    case "team" :: search if search.nonEmpty =>
      searchData(Teams.loadData, search.mkString(" "))

    case "title" :: search if search.nonEmpty =>
      searchData(Titles.loadData, search.mkString(" "))

    case "get" :: "member" :: id if id.nonEmpty =>
      findDataById(Members.loadData, id.mkString(" "))

    case "get" :: "team" :: id if id.nonEmpty =>
      findDataById(Teams.loadData, id.mkString(" "))

    case "get" :: "title" :: id if id.nonEmpty =>
      findDataById(Titles.loadData, id.mkString(" "))

    case "find" :: "member" :: field :: value if value.nonEmpty =>
      findDataByField(Members.loadData, field, value.mkString(" "))

    case "add" :: "member" :: id if id.nonEmpty =>
      updatingMembers { (data, validRefs) =>
        OrgData.add(data, id.mkString(" "), validRefs)
      }

    case "update" :: "member" :: id :: field :: Nil =>
      updatingMembers { (data, validRefs) =>
        OrgData.update(data, id, Overwrite, field, validRefs)
      }

    case "update" :: "member" :: id :: action :: field :: Nil =>
      updatingMembers { (data, validRefs) =>
        OrgData.update(data, id, MultiValueFieldAction(action), field, validRefs)
      }

    case "remove" :: "member" :: id =>
      updatingMembers { (data, validRefs) =>
        OrgData.remove(data, id.mkString(" "))
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
      |  member|team|title [search] # E.g. title principal; member john foo
      |  get member|team|title [id] # E.g. get member john foo; get team bar
      |  find member [field] [value]
      |  add member [id]
      |  update member [id] [field]
      |  update member [id] add|remove [field] #For multi-value fields
      |  remove member [id]
      |  validate
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

    if (validValues.nonEmpty && !value.isEmpty && !validValues.contains(value)) {
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

  lazy val filePath = OrgDataDir + filename

  def loadData: Csv = Csv.read(filePath)
  def writeData(data: Csv): Unit = Csv.write(filePath, data)
}

case class ValidRefs(members: Seq[String], teams: Seq[String], titles: Seq[String]) {
  def forField(field: Field): Seq[String] = {
    if (field.isMemberRef) members
    else if (field.isTeamRef) teams
    else if (field.isTitleRef) titles
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
      if (field.isId) id
      else
        CmdLineUtils.promptForValue(field.name, field.isMultiValue, validRefs.forField(field))
    }

    currentData.addRow(newRow)
  }

  def update(currentData: Csv, id: String, action: FieldAction, fieldName: String, validRefs: ValidRefs): Csv = {
    val field = currentData.header.field(fieldName)
    val value = CmdLineUtils.promptForValue(field.name, field.isMultiValue, validRefs.forField(field))
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
      if (cell.field.isMemberRef && !cell.value.isEmpty && !validRefs.members.contains(cell.value)) sys.error(s"Invalid member reference [${cell.value}] in row [${row.index+1}]")

      if (cell.field.isTeamRef && !cell.value.isEmpty && !validRefs.teams.contains(cell.value)) sys.error(s"Invalid team reference [${cell.value}] in row [${row.index+1}]")

      if (cell.field.isTitleRef && !cell.value.isEmpty && !validRefs.titles.contains(cell.value)) sys.error(s"Invalid title reference [${cell.value}] in row [${row.index+1}]")
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
  val Delimiter = '|'
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
    (currentValue.split(Delimiter) :+ change).mkString(Delimiter.toString)
  }
}

case object MultiValueRemove extends MultiValueFieldAction {
  override def update(currentValue: String, change: String) = {
    currentValue.split(Delimiter).filterNot(_ == change).mkString(Delimiter.toString)
  }
}

case class Field(name: String, isMultiValue: Boolean, isId: Boolean, isMemberRef: Boolean, isTeamRef: Boolean, isTitleRef: Boolean) {
  override def toString = {
    if (isMultiValue)
      name + " " + Field.MultiValueIndicator
    else if (isId)
      name + " " + Field.IdIndicator
    else if (isMemberRef)
      name + " " + Field.MemberRefIndicator
    else if (isTeamRef)
      name + " " + Field.TeamRefIndicator
    else if (isTitleRef)
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

    val fieldName = header.split('(').head.trim

    Field(
      name = fieldName,
      isMultiValue = multiValueIndicatorIndex != -1,
      isId = idIndicatorIndex != -1,
      isMemberRef = memberRefIndicatorIndex != -1,
      isTeamRef = teamRefIndicatorIndex != -1,
      isTitleRef = titleRefIndicatorIndex != -1
    )
  }
}

case class Header(fields: Seq[Field]) {
  val idField = fields.toList.filter(_.isId) match {
    case singleId :: Nil => singleId
    case _ => sys.error("Header should contain one and only one id field")
  }

  def field(fieldName: String): Field = {
    fields.find { _.name.equalsIgnoreCase(fieldName) }.getOrElse {
      sys.error(s"Unknown field [$fieldName]. Choose from [${fields.map(_.name).mkString(", ")}]")
    }
  }
}

case class Cell(field: Field, value: String) {
  lazy val toJson = {
    val jsonFriendlyValue = value.replaceAllLiterally("\"", "")
    s""""${field.name}": "$jsonFriendlyValue""""
  }
}

case class Row(index: Int, cells: Seq[Cell]) {

  val id = cells.toList.filter(_.field.isId) match {
    case singleId :: Nil if singleId.value.nonEmpty => singleId.value
    case _ => sys.error("Row has no id")
  }

  def cell(fieldName: String): Cell = {
    cells.find(_.field.name.equalsIgnoreCase(fieldName)).getOrElse {
      sys.error(s"Unknown field [$fieldName]. Choose from [${cells.map(_.field.name).mkString(", ")}]")
    }
  }

  lazy val toJson = s"""{ ${cells.map(_.toJson).mkString(", ")} }"""
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

  if (ids.size != rows.size) sys.error(s"Ids are not unique. There are [${ids.size}] ids and [${rows.size}] rows.")

  // Returns the row with the given id
  def row(id: String): Row = {
    rows.find { _.id.equalsIgnoreCase(id) }.getOrElse {
      val chooseFrom = if (ids.size < 20) s" Choose from [${ids.sorted.mkString(", ")}]." else ""
      sys.error(s"Unknown ${header.idField.name} [$id].$chooseFrom")
    }
  }

  // Returns the id of all matching rows
  def findRows(fieldName: String, value: String): Seq[Row] = {
    rows.filter { _.cell(fieldName).value.toLowerCase.contains(value.toLowerCase) }
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

  def write(filePath: String, csv: Csv): Unit = {
    val header = csv.header.fields.mkString(Separator)
    val rows = csv.rows.map { _.cells.map(_.value).mkString(Separator) }

    safelyWrite(filePath, header +: rows)
  }

  def read(filePath: String): Csv = {
    safelyRead(filePath) { lines =>
      if (lines.hasNext) {
        val headerLine = parseLine(lines.next())
        val headerFields = headerLine.distinct
        if (headerLine.size != headerFields.size) sys.error("There are duplicate names in the header")
        if (headerFields.exists(_.isEmpty)) sys.error("The header has empty fields")

        val header = Header(headerFields.map { Field(_) })

        val rows = lines.zipWithIndex.map {
          case (line, index) =>
            val values = parseLine(line)
            if (values.size != headerFields.size) sys.error(s"Row [${index+1}] does not have the same number of fields [${values.size}] as the header [${headerFields.size}]")
            Row(header, index, values)
        }.toSeq
        Csv(header, rows)
      } else {
        sys.error(s"File [$filePath] is missing a header")
      }
    }
  }

  // Closes the file after use
  private def safelyRead[T](filePath: String)(f: Iterator[String] => T) = {
    val source = io.Source.fromFile(filePath)
    val attemptToProcess = Try { f(source.getLines()) }
    source.close()
    attemptToProcess.get
  }

  // Closes the file after use
  private def safelyWrite(filePath: String, rows: Seq[String]): Unit = {
    var bw = new BufferedWriter(new FileWriter(filePath, false))
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

  private def parseLine(line: String): Seq[String] = {
    // Have to support fields wrapped in quotes that contain commas
    val CommaRegex = ",(?=([^\"]*\"[^\"]*\")*[^\"]*$)"

    // Annoyingly, split ignores trailing commas
    val elements =
      if (line.endsWith(","))
        line.split(CommaRegex) :+ ""
      else
        line.split(CommaRegex)

    elements.map(_.trim)
  }
}



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

case class MemberKey(name: String)

case class Member(name: MemberKey, email: Seq[String], github: Option[String], title: TitleKey, manager: Option[MemberKey],
                  team: Option[TeamKey])

case class TitleKey(title: String, group: String)

case class Title(key: TitleKey, level: BigDecimal, track: String)

case class TeamKey(name: String)

case class Team(name: TeamKey, lead: MemberKey, pmo: Option[MemberKey], product: Option[MemberKey])


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
  val memberData = Members.loadCsv
  val teamIds = if (memberData.header.fields.exists(_.isTeamRef)) Teams.loadCsv.ids else Nil
  val titleIds = if (memberData.header.fields.exists(_.isTitleRef)) Titles.loadCsv.ids else Nil

  val updatedData = f(memberData, ValidRefs(memberData.ids, teamIds, titleIds))

  Members.writeData(updatedData)

  // If we're not in test mode then commit the change with a consistent message for easy parsing afterwards
  if (!TestMode)
    Seq("sh", "-c", s"""cd $OrgDataDir; git commit -am "${args.mkString(" ")}"; cd - """).!
}

def execute(): Unit = {
  args.toList match {
    case "member" :: search if search.nonEmpty =>
      searchData(Members.loadCsv, search.mkString(" "))

    case "team" :: search if search.nonEmpty =>
      searchData(Teams.loadCsv, search.mkString(" "))

    case "title" :: search if search.nonEmpty =>
      searchData(Titles.loadCsv, search.mkString(" "))

    case "get" :: "member" :: id if id.nonEmpty =>
      findDataById(Members.loadCsv, id.mkString(" "))

    case "get" :: "team" :: id if id.nonEmpty =>
      findDataById(Teams.loadCsv, id.mkString(" "))

    case "get" :: "title" :: id if id.nonEmpty =>
      findDataById(Titles.loadCsv, id.mkString(" "))

    case "find" :: "member" :: field :: value if value.nonEmpty =>
      findDataByField(Members.loadCsv, field, value.mkString(" "))

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

      val teamsCsv = Teams.loadCsv

      OrgData.validate(membersData, titlesData.map(_.key), teamsData.map(_.name))
      OrgData.validateTeams(teamsCsv, membersData.map(_.name))
      OrgData.validate(titlesData)

    case _ =>
      sys.error(CmdLineUtils.Usage)
  }
}

object CmdLineUtils {
  import io.StdIn._

  val Usage =
    """
      |Usage:
      |  member|team|title [search] -- E.g. title principal; member john foo
      |  get member|team|title [id] -- E.g. get member john foo; get team bar
      |  find member [field] [value] -- E.g. find member github foobar
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

  protected def nonEmptyStringOrOption(value: String): Option[String] = {
    value match {
      case "" => None
      case v => Some(v)
    }
  }

  def loadCsv: Csv = Csv.read(filePath)
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

  private def parseRow(row: Row): Member = {
    Member(MemberKey(row.cell("name").value),
      row.cell("email").value.split("|").toSeq,
      nonEmptyStringOrOption(row.cell("github").value),
      TitleKey(row.cell("title").value,
        row.cell("group").value)
      ,
      nonEmptyStringOrOption(row.cell("manager").value).map(MemberKey.apply),
      nonEmptyStringOrOption(row.cell("team").value).map(TeamKey.apply))
  }

  def loadData: Seq[Member] = {
    loadCsv.rows.map(parseRow)
  }
}

object Teams extends OrgData {
  override val filename = "teams.csv"

  private def parseRow(row: Row): Team = {
    Team(
      TeamKey(row.cell("name").value),
      MemberKey(row.cell("lead").value),
      nonEmptyStringOrOption(row.cell("pmo").value).map(MemberKey.apply),
      nonEmptyStringOrOption(row.cell("product").value).map(MemberKey.apply)
    )
  }

  def loadData: Seq[Team] = {
    loadCsv.rows.map(parseRow)
  }

}

object Titles extends OrgData {
  override val filename = "titles.csv"

  private def parseRow(row: Row): Title = {
    Title(
      TitleKey(row.cell("Title").value, row.cell("Group").value),
      BigDecimal(row.cell("Level").value),
      row.cell("Track").value
    )
  }

  def loadData: Seq[Title] = {
    loadCsv.rows.map(parseRow)
  }
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

  def validate(members: Seq[Member], titles: Seq[TitleKey], teams: Seq[TeamKey]): Unit = {
    // Validate titles
    for {
      (member, index) <- members.zipWithIndex
    } {
      if (!titles.contains(member.title)) {
        sys.error(s"members.csv: Invalid title reference ${member.title} in row ${index + 1}, member ${member.name}")
      }
    }

    // Validate teams
    for {
      (member, index) <- members.zipWithIndex
    } {
      if (member.team.isDefined && !teams.contains(member.team.get)) {
        sys.error(s"members.csv: Invalid team reference ${member.team} in row ${index + 1}, member ${member.name}")
      }
    }
  }

  def validate(titles: Seq[Title]): Unit = {
    // Titles should be unique within a group
    val titleGroups = titles.groupBy(_.key.group)
    titleGroups.foreach {
      case (group, titles) =>
        val nonUniqueTitles = titles.groupBy(_.key.title).filter(_._2.length > 1)
        if (!nonUniqueTitles.isEmpty) {
          sys.error(s"Non unique titles exist for group $group: ${nonUniqueTitles.map(_._1).mkString(", ")}")
        }
    }
  }

  def validateTeams(data: Csv, members: Seq[MemberKey]): Unit = {
    // Check the integrity of references
    for {
      row <- data.rows
      cell <- row.cells
    } {
      if (cell.field.isMemberRef && !cell.value.isEmpty && !members.contains(MemberKey(cell.value))) sys.error(s"${data.filename}: Invalid member reference [${cell.value}] in row [${row.index+1}]")
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

case class Csv(filename: String, header: Header, rows: Seq[Row]) {

  val ids: Seq[String] = rows.map(_.id).distinct

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
    val filename = {
      val lastSlash = filePath.lastIndexOf('/') + 1
      filePath.substring(lastSlash)
    }

    safelyRead(filePath) { lines =>
      if (lines.hasNext) {
        val headerLine = parseLine(lines.next())
        val headerFields = headerLine.distinct
        if (headerLine.size != headerFields.size) sys.error(s"$filename: There are duplicate names in the header")
        if (headerFields.exists(_.isEmpty)) sys.error(s"$filename: The header has empty fields")

        val header = Header(headerFields.map { Field(_) })

        val rows = lines.zipWithIndex.map {
          case (line, index) =>
            val values = parseLine(line)
            if (values.size != headerFields.size) sys.error(s"$filename: Row [${index+1}] does not have the same number of fields [${values.size}] as the header [${headerFields.size}]")
            Row(header, index, values)
        }.toList
        Csv(filename, header, rows)
      } else {
        sys.error(s"$filename: Missing header")
      }
    }
  }

  // Closes the file after use
  private def safelyRead[T](filePath: String)(f: Iterator[String] => T) = {
    val source = io.Source.fromFile(filePath)
    try {
      f(source.getLines())
    } finally {
      source.close()
    }
  }

  // Closes the file after use
  private def safelyWrite(filePath: String, rows: Seq[String]): Unit = {
    val bw = new BufferedWriter(new FileWriter(filePath, false))
    Try {
      rows.foreach { row =>
        bw.write(row)
        bw.newLine()
      }
      bw.flush()
    }

    Try { bw.close() }
  }

  private def parseLine(line: String): Seq[String] = {
    // Have to support fields wrapped in quotes that contain commas
    val CommaRegex = ",(?=([^\"]*\"[^\"]*\")*[^\"]*$)"

    // Annoyingly, split ignores trailing commas
    val numTrailingCommas = line.length - line.reverse.dropWhile(_ == ',').length
    val elements = line.split(CommaRegex) ++ Array.fill(numTrailingCommas)("")

    elements.map(_.trim)
  }
}



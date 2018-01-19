#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

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

def list(data: Csv): Unit = {
  Console.println(data.rows.sortBy(_.id).map(_.toJson).mkString("[ ", ", ", " ]"))
}

def listIds(data: Csv): Unit = {
  Console.println(data.ids.sorted.mkString("[ \"", "\", \"", "\" ]"))
}

def execute(): Unit = {
  args.toList match {
    case "memberids" :: Nil =>
      listIds(Members.loadData)

    case "members" :: Nil =>
      list(Members.loadData)

    case "members" :: search =>
      searchData(Members.loadData, search.mkString(" "))

    case "teamids" :: Nil =>
      listIds(Teams.loadData)

    case "teams" :: Nil =>
      list(Teams.loadData)

    case "teams" :: search =>
      searchData(Teams.loadData, search.mkString(" "))

    case "titleids" :: Nil =>
      listIds(Titles.loadData)

    case "titles" :: Nil =>
      list(Titles.loadData)

    case "titles" :: search =>
      searchData(Titles.loadData, search.mkString(" "))

    case "get" :: "member" :: id if id.nonEmpty =>
      findDataById(Members.loadData, id.mkString(" "))

    case "get" :: "team" :: id if id.nonEmpty =>
      findDataById(Teams.loadData, id.mkString(" "))

    case "get" :: "title" :: id if id.nonEmpty =>
      findDataById(Titles.loadData, id.mkString(" "))

    case "find" :: "members" :: field :: value if value.nonEmpty =>
      findDataByField(Members.loadData, field, value.mkString(" "))

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
  val Usage =
    """
      |Usage:
      |  members|teams|titles [search] -- E.g. titles principal; members john foo
      |  get member|team|title [id] -- E.g. get member john foo; get team bar
      |  find members [field] [value] -- E.g. find members github foobar
    """.stripMargin
}


// Org data

trait OrgData {
  def filename: String

  lazy val filePath = OrgDataDir + filename

  def loadData: Csv = Csv.read(filePath)
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
  def validate(data: Csv, validRefs: ValidRefs): Unit = {
    def validRef(value: String, validValues: Seq[String]): Boolean = value.isEmpty || validValues.contains(value)

    // Check the integrity of references
    for {
      row <- data.rows
      cell <- row.cells
    } {
      if (cell.field.isMemberRef && !validRef(cell.value, validRefs.members))
        sys.error(s"${data.filename}: Invalid member reference [${cell.value}] in row [${row.index+1}]")

      if (cell.field.isTeamRef && !validRef(cell.value, validRefs.teams))
        sys.error(s"${data.filename}: Invalid team reference [${cell.value}] in row [${row.index+1}]")

      if (cell.field.isTitleRef && !validRef(cell.value, validRefs.titles))
        sys.error(s"${data.filename}: Invalid title reference [${cell.value}] in row [${row.index+1}]")
    }
  }
}


// Csv models


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

  if (ids.size != rows.size) sys.error(s"$filename: Ids are not unique. There are [${ids.size}] ids and [${rows.size}] rows.")

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
}

object Csv {

  private val Separator = ","

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
        }.toSeq
        Csv(filename, header, rows)
      } else {
        sys.error(s"$filename: Missing header")
      }
    }
  }

  // Closes the file after use
  private def safelyRead[T](filePath: String)(f: Iterator[String] => T): T = {
    val source = io.Source.fromFile(filePath)
    val attemptToProcess = Try { f(source.getLines()) }
    source.close()
    attemptToProcess.get
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



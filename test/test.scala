#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import java.io.{BufferedWriter, File, FileWriter}

import scala.language.postfixOps
import scala.sys.process._
import scala.util.Try


testValidate("validateNoIdField",
  memberFileContents = Seq("Foo,Bar"),
  expectedError = "Header should contain one and only one id field"
)

testValidate("validateMoreThanOneIdField",
  memberFileContents = Seq("Foo (id),Bar (id)"),
  expectedError = "Header should contain one and only one id field"
)

testValidate("validateEmptyHeaderField",
  memberFileContents = Seq("Foo (id),"),
  expectedError = "members.csv: The header has empty fields"
)

testValidate("validateUniqueFieldNames",
  memberFileContents = Seq("Foo,Bar,Foo"),
  expectedError = "members.csv: There are duplicate names in the header"
)

testValidate("validateNoId",
  memberFileContents = Seq("Foo (id),Bar",",b"),
  expectedError = "Row has no id"
)

testValidate("validateDuplicateIds",
  memberFileContents = Seq("Foo (id),Bar","foo,b","foo,c"),
  expectedError = "members.csv: Ids are not unique. There are [1] ids and [2] rows."
)

testValidate("validateNumberRowCells",
  memberFileContents = Seq("Foo (id),Bar","foo,b","foo,c,d"),
  expectedError = "members.csv: Row [2] does not have the same number of fields [3] as the header [2]"
)

testValidate("validateCellContentsWithQuotes",
  memberFileContents = Seq("Foo (id),Bar","foo,\"b,c\"")
)

testValidate("validateCellContentsWithTrailingCommas",
  memberFileContents = Seq("Foo (id),Bar,Baz","foo,,")
)

testValidate("validateMemberRefs",
  memberFileContents = Seq("Foo (id),Foo ref (member)", "f1,", "f2,f3"),
  expectedError = "members.csv: Invalid member reference [f3] in row [2]"
)

testValidate("validateTeamRefs",
  memberFileContents = Seq("Foo (id),Team ref (team)","f1,t2"),
  teamFileContents = Seq("Team (id)", "t1"),
  expectedError = "members.csv: Invalid team reference [t2] in row [1]"
)

testValidate("validateTitleRefs",
  memberFileContents = Seq("Foo (id),Title ref (title)","f1,t2"),
  titleFileContents = Seq("Title (id)", "t1"),
  expectedError = "members.csv: Invalid title reference [t2] in row [1]"
)

testValidate("validateGoodData",
  memberFileContents = Seq("Member (id),Member ref (member),Team ref (team),Title ref (title)", "m1,,team1,title1", "m2,m1,team2,title2"),
  teamFileContents = Seq("Team (id)", "team1", "team2"),
  titleFileContents = Seq("Title (id)", "title1", "title2")
)

testRead("testGetMember",
  memberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """{ "Member": "m1", "Member Foo": "mfoo1" }""") { testName =>

  runReadCommand("""../org.scala get member m1""")
}

testRead("testGetTeam",
  teamFileContents = Seq("Team (id),Team Foo", "t1,tfoo1", "t2,tfoo2"),
  expected = """{ "Team": "t1", "Team Foo": "tfoo1" }""") { testName =>

  runReadCommand("""../org.scala get team t1""")
}

testRead("testGetTitle",
  titleFileContents = Seq("Title (id),Title Foo", "t1,tfoo1", "t2,tfoo2"),
  expected = """{ "Title": "t1", "Title Foo": "tfoo1" }""") { testName =>

  runReadCommand("""../org.scala get title t1""")
}

testRead("testGetWithCommas",
  memberFileContents = Seq("Member (id),Member Foo", "m1,\"mfoo1,2\"", "m2,mfoo2"),
  expected = """{ "Member": "m1", "Member Foo": "mfoo1,2" }""") { testName =>

  runReadCommand("""../org.scala get member m1""")
}

testRead("testFindMember",
  memberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """[ { "Member": "m2", "Member Foo": "mfoo2" } ]""") { testName =>

  runReadCommand("""../org.scala find members "Member Foo" mfoo2""")
}

testRead("testFindMemberMultiple",
  memberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """[ { "Member": "m1", "Member Foo": "mfoo1" }, { "Member": "m2", "Member Foo": "mfoo2" } ]""") { testName =>

  runReadCommand("""../org.scala find members "Member Foo" mfoo""")
}

testRead("testListMemberIds",
  memberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """[ "m1", "m2" ]""") { testName =>

  runReadCommand("""../org.scala memberids""")
}

testRead("testListMembers",
  memberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """[ { "Member": "m1", "Member Foo": "mfoo1" }, { "Member": "m2", "Member Foo": "mfoo2" } ]""") { testName =>

  runReadCommand("""../org.scala members""")
}

testRead("testListTeamIds",
  teamFileContents = Seq("Team (id),Team Foo", "t1,tfoo1", "t2,tfoo2"),
  expected = """[ "t1", "t2" ]""") { testName =>

  runReadCommand("""../org.scala teamids""")
}

testRead("testListTeams",
  teamFileContents = Seq("Team (id),Team Foo", "t1,tfoo1", "t2,tfoo2"),
  expected = """[ { "Team": "t1", "Team Foo": "tfoo1" }, { "Team": "t2", "Team Foo": "tfoo2" } ]""") { testName =>

  runReadCommand("""../org.scala teams""")
}

testRead("testListTitleIds",
  titleFileContents = Seq("Title (id),Title Foo", "t1,tfoo1", "t2,tfoo2"),
  expected = """[ "t1", "t2" ]""") { testName =>

  runReadCommand("""../org.scala titleids""")
}

testRead("testListTitles",
  titleFileContents = Seq("Title (id),Title Foo", "t1,tfoo1", "t2,tfoo2"),
  expected = """[ { "Title": "t1", "Title Foo": "tfoo1" }, { "Title": "t2", "Title Foo": "tfoo2" } ]""") { testName =>

  runReadCommand("""../org.scala titles""")
}

testRead("testSearchMemberNoResults",
  memberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """[  ]""") { testName =>

  runReadCommand("""../org.scala members bar""")
}

testRead("testSearchMemberSingleResult",
  memberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """[ { "Member": "m2", "Member Foo": "mfoo2" } ]""") { testName =>

  runReadCommand("""../org.scala members mfoo2""")
}

testRead("testSearchMemberMultipleResults",
  memberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """[ { "Member": "m1", "Member Foo": "mfoo1" }, { "Member": "m2", "Member Foo": "mfoo2" } ]""") { testName =>

  runReadCommand("""../org.scala members mfoo""")
}

def writeFile(filePath: String, rows: Seq[String]): Unit = {
  var bw = new BufferedWriter(new FileWriter(filePath, false))
  rows.foreach { row =>
    bw.write(row)
    bw.newLine()
  }
  bw.flush()
  bw.close()
}

def deleteFile(filePath: String): Unit = {
  new File(filePath).delete()
}

def validate[T](testName: String, expected: T, actual: T): Unit = {
  require(actual == expected, s"$testName FAILED: $actual was not equal to $expected")
  println(s"$testName PASSED")
}

def validateFileContents(testName: String, filePath: String, expected: Seq[String]): Unit = {
  val lines = io.Source.fromFile(filePath).getLines().toList
  validate(testName, expected, lines)
}

/**
  * Runs a command
  * @param command The command to run
  * @return STDOUT
  */
def runCommand(command: String, stdout: StringBuilder = new StringBuilder, stderr: StringBuilder = new StringBuilder): Int = {
  Seq("sh", "-c", testMode(command)) ! scala.sys.process.ProcessLogger(stdout append _, stderr append _)
}

/**
  * Runs a command and returns the output from STDOUT
  * @param command The command to run
  * @return STDOUT
  */
def runReadCommand(command: String): String = {
  val result = Seq("sh", "-c", testMode(command)) !!

  result.trim
}

def testMode(command: String): String = "TEST_MODE=1 " + command

// Run a test against a 'read' command
def testRead(testName: String,
             memberFileContents: Seq[String] = Nil,
             teamFileContents: Seq[String] = Nil,
             titleFileContents: Seq[String] = Nil,
             expected: String)
            (runTest: String => String): Unit = {
  val test = Try {
    setup(memberFileContents, teamFileContents, titleFileContents)

    val result = runTest(testName)

    validate(testName, expected, result)
  }

  cleanup()

  test.get
}

def testValidate(testName: String,
                 memberFileContents: Seq[String] = Seq("Foo (id),Bar"),
                 teamFileContents: Seq[String] = Seq("Foo (id),Bar"),
                 titleFileContents: Seq[String] = Seq("Foo (id),Bar"),
                 expectedError: String = ""): Unit = {
  val test = Try {
    setup(memberFileContents, teamFileContents, titleFileContents)

    val stderr = new StringBuilder
    runCommand("""../org.scala validate""", stderr = stderr)
    validate(testName, expectedError, stderr.mkString)
  }

  cleanup()

  test.get
}

def setup(initialMemberFileContents: Seq[String],
          initialTeamFileContents: Seq[String],
          initialTitleFileContents: Seq[String]): Unit = {
  if (initialMemberFileContents.nonEmpty)
    writeFile("members.csv", initialMemberFileContents)

  if (initialTeamFileContents.nonEmpty)
    writeFile("teams.csv", initialTeamFileContents)

  if (initialTitleFileContents.nonEmpty)
    writeFile("titles.csv", initialTitleFileContents)
}

def cleanup(): Unit = {
  deleteFile("members.csv")
  deleteFile("teams.csv")
  deleteFile("titles.csv")
}

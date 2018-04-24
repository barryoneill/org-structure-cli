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
  expectedError = "The header has empty fields"
)

testValidate("validateUniqueFieldNames",
  memberFileContents = Seq("Foo,Bar,Foo"),
  expectedError = "There are duplicate names in the header"
)

testValidate("validateNoId",
  memberFileContents = Seq("Foo (id),Bar",",b"),
  expectedError = "Row has no id"
)

testValidate("validateDuplicateIds",
  memberFileContents = Seq("Foo (id),Bar","foo,b","foo,c"),
  expectedError = "Ids are not unique. There are [1] ids and [2] rows."
)

testValidate("validateNumberRowCells",
  memberFileContents = Seq("Foo (id),Bar","foo,b","foo,c,d"),
  expectedError = "Row [2] does not have the same number of fields [3] as the header [2]"
)

testValidate("validateCellContentsWithCommas",
  memberFileContents = Seq("Foo (id),Bar","foo,\"b,c\"")
)

testValidate("validateMemberRefs",
  memberFileContents = Seq("Foo (id),Foo ref (member)", "f1,", "f2,f3"),
  expectedError = "Invalid member reference [f3] in row [2]"
)

testValidate("validateTeamRefs",
  memberFileContents = Seq("Foo (id),Team ref (team)","f1,t2"),
  teamFileContents = Seq("Team (id)", "t1"),
  expectedError = "Invalid team reference [t2] in row [1]"
)

testValidate("validateTitleRefs",
  memberFileContents = Seq("Foo (id),Title ref (title)","f1,t2"),
  titleFileContents = Seq("Title (id)", "t1"),
  expectedError = "Invalid title reference [t2] in row [1]"
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
  expected = """[ "m2" ]""") { testName =>

  runReadCommand("""../org.scala find member "Member Foo" mfoo2""")
}

testRead("testFindMemberMultiple",
  memberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """[ "m1", "m2" ]""") { testName =>

  runReadCommand("""../org.scala find member "Member Foo" mfoo""")
}

testUpdateMembers("addSimpleMember",
  initialMemberFileContents = Seq("Member (id),Member Foo"),
  expectedFileContents = Seq("Member (id),Member Foo", "m1,mfoo")) { () =>

  runUpdateCommand(
    command = """../org.scala add member m1""",
    promptValues = Seq("mfoo")
  )
}

testUpdateMembers("addSimpleMemberWithEmpty",
  initialMemberFileContents = Seq("Member (id),Member Foo,Member Bar"),
  expectedFileContents = Seq("Member (id),Member Foo,Member Bar", "m1,,mbar")) { () =>

  runUpdateCommand(
    command = """../org.scala add member m1""",
    promptValues = Seq("", "mbar")
  )
}

testUpdateMembers("addMemberRef",
  initialMemberFileContents = Seq("Member (id),Member Ref (member)", "m1,"),
  expectedFileContents = Seq("Member (id),Member Ref (member)", "m1,", "m2,m1")) { () =>

  runUpdateCommand(
    command = """../org.scala add member m2""",
    promptValues = Seq("m1")
  )
}

testUpdateMembers("addEmptyMemberRef",
  initialMemberFileContents = Seq("Member (id),Member Ref (member)"),
  expectedFileContents = Seq("Member (id),Member Ref (member)", "m1,")) { () =>

  runUpdateCommand(
    command = """../org.scala add member m1""",
    promptValues = Seq("")
  )
}

testUpdateMembers("addMemberRefWithPrompt",
  initialMemberFileContents = Seq("Member (id),Member Ref (member)", "m1,"),
  expectedFileContents = Seq("Member (id),Member Ref (member)", "m1,", "m2,m1")) { () =>

  runUpdateCommand(
    command = """../org.scala add member m2""",
    promptValues = Seq("m", "1")
  )
}

testUpdateMembers("addTeamRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t1")) { () =>

  runUpdateCommand(
    command = """../org.scala add member m1""",
    promptValues = Seq("mfoo", "t1")
  )
}

testUpdateMembers("addEmptyTeamRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,")) { () =>

  runUpdateCommand(
    command = """../org.scala add member m1""",
    promptValues = Seq("mfoo", "")
  )
}

testUpdateMembers("addTeamRefWithPrompt",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t1")) { () =>

  // An invalid team should prompt to enter one from a numbered list
  runUpdateCommand(
    command = """../org.scala add member m1""",
    promptValues = Seq("mfoo", "t2", "1")
  )
}

testUpdateMembers("addTitleRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Title Ref (title)"),
  initialTitleFileContents = Seq("Title (id),Title Foo", "t1,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t1")) { () =>

  runUpdateCommand(
    command = """../org.scala add member m1""",
    promptValues = Seq("mfoo", "t1")
  )
}

testUpdateMembers("addTitleRefWithPrompt",
  initialMemberFileContents = Seq("Member (id),Member Foo,Title Ref (title)"),
  initialTitleFileContents = Seq("Title (id),Title Foo", "t1,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t1")) { () =>

  // An invalid title should prompt to enter one from a numbered list
  runUpdateCommand(
    command = """../org.scala add member m1""",
    promptValues = Seq("mfoo", "t2", "1")
  )
}

testUpdateMembers("updateSimpleMember",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1"),
  expectedFileContents = Seq("Member (id),Member Foo", "m1,mfoo2")) { () =>

  runUpdateCommand(
    command = """../org.scala update member m1 "Member Foo"""",
    promptValues = Seq("mfoo2")
  )
}

testUpdateMembers("updateMemberAddMultiValue",
  initialMemberFileContents = Seq("Member (id),Member Foo (multi)", "m1,mfoo1"),
  expectedFileContents = Seq("Member (id),Member Foo (multi)", "m1,mfoo1|mfoo2")) { () =>

  runUpdateCommand(
    command = """../org.scala update member m1 add "Member Foo"""",
    promptValues = Seq("mfoo2")
  )
}

testUpdateMembers("updateMemberRemoveMultiValue",
  initialMemberFileContents = Seq("Member (id),Member Foo (multi)", "m1,mfoo1|mfoo2"),
  expectedFileContents = Seq("Member (id),Member Foo (multi)", "m1,mfoo2")) { () =>

  runUpdateCommand(
    command = """../org.scala update member m1 remove "Member Foo"""",
    promptValues = Seq("mfoo1")
  )
}

testUpdateMembers("updateTeamRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t1"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo", "t2,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t2")) { () =>

  runUpdateCommand(
    command = """../org.scala update member m1 "Team Ref"""",
    promptValues = Seq("t2")
  )
}

testUpdateMembers("updateTeamRefWithPrompt",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t1"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo", "t2,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t2")) { () =>

  runUpdateCommand(
    command = """../org.scala update member m1 "Team Ref"""",
    promptValues = Seq("t3", "2")
  )
}

testUpdateMembers("updateTitleRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t1"),
  initialTitleFileContents = Seq("Title (id),Title Foo", "t1,tfoo", "t2,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t2")) { () =>

  runUpdateCommand(
    command = """../org.scala update member m1 "Title Ref"""",
    promptValues = Seq("t2")
  )
}

testUpdateMembers("updateTitleRefWithPrompt",
  initialMemberFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t1"),
  initialTitleFileContents = Seq("Title (id),Title Foo", "t1,tfoo", "t2,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t2")) { () =>

  runUpdateCommand(
    command = """../org.scala update member m1 "Title Ref"""",
    promptValues = Seq("t3", "2")
  )
}

testUpdateMembers("removeMember",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expectedFileContents = Seq("Member (id),Member Foo", "m2,mfoo2")) { () =>

  runUpdateCommand(
    command = """../org.scala remove member m1"""
  )
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

/**
  * Runs a command. STDOUT is sent to /dev/null. STDERR is sent to the console.
  * @param command      The command to run
  * @param promptValues Values to input for the prompts that are expected
  * @return The exit code
  */
def runUpdateCommand(command: String, promptValues: Seq[String] = Nil): Int = {
  (s"printf ${promptValues.mkString("", """\n""", """\n""")}" #| Seq("sh", "-c", testMode(command)) #> new File("/dev/null")) !
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

// Run a test against a members 'write' command. runTest() should return the expected contents of the members.csv file
// after the test has completed.
def testUpdateMembers(testName: String,
                      initialMemberFileContents: Seq[String],
                      initialTeamFileContents: Seq[String] = Nil,
                      initialTitleFileContents: Seq[String] = Nil,
                      expectedFileContents: Seq[String])
                     (runTest: () => Unit): Unit = {
  val test = Try {
    setup(initialMemberFileContents, initialTeamFileContents, initialTitleFileContents)

    runTest()

    validateFileContents(testName, "members.csv", expectedFileContents)
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
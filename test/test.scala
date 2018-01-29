#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import java.io.{BufferedWriter, File, FileWriter}

import scala.sys.process._
import scala.util.Try

testReadMembers("testGetMember",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """{"Member":"m1","Member Foo":"mfoo1"}""") { testName =>

  runReadCommand("""../org.scala get member m1""")
}

testReadMembers("testFindMember",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = """m2""") { testName =>

  runReadCommand("""../org.scala find member "Member Foo" mfoo2""")
}

testReadMembers("testFindMemberMultiple",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2"),
  expected = "m1\nm2") { testName =>

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

testUpdateMembers("addTeamRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t1")) { () =>

  runUpdateCommand(
    command = """../org.scala add member m1""",
    promptValues = Seq("mfoo", "t1")
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

testUpdateMembers("updateTitleRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t1"),
  initialTitleFileContents = Seq("Title (id),Title Foo", "t1,tfoo", "t2,tfoo"),
  expectedFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t2")) { () =>

  runUpdateCommand(
    command = """../org.scala update member m1 "Title Ref"""",
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
  * Runs a command and returns the output from STDOUT
  * @param command The command to run
  * @return STDOUT
  */
def runReadCommand(command: String): String = {
  Seq("sh", "-c", testMode(command)).!!.trim
}

/**
  * Runs a command. STDOUT is sent to /dev/null. STDERR is sent to the console.
  * @param command      The command to run
  * @param promptValues Values to input for the prompts that are expected
  * @return The exit code
  */
def runUpdateCommand(command: String, promptValues: Seq[String] = Nil): Int = {
  (s"printf ${promptValues.mkString("", """\n""", """\n""")}" #| Seq("sh", "-c", testMode(command)) #> new File("/dev/null")).!
}

def testMode(command: String): String = "TEST_MODE=1 " + command

// Run a test against a members 'read' command
def testReadMembers(testName: String,
                    initialMemberFileContents: Seq[String],
                    initialTeamFileContents: Seq[String] = Nil,
                    initialTitleFileContents: Seq[String] = Nil,
                    expected: String)
                   (runTest: String => String): Unit = {
  val test = Try {
    if (initialMemberFileContents.nonEmpty)
      writeFile("members.csv", initialMemberFileContents)

    if (initialTeamFileContents.nonEmpty)
      writeFile("teams.csv", initialTeamFileContents)

    if (initialTitleFileContents.nonEmpty)
      writeFile("titles.csv", initialTitleFileContents)

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
    if (initialMemberFileContents.nonEmpty)
      writeFile("members.csv", initialMemberFileContents)

    if (initialTeamFileContents.nonEmpty)
      writeFile("teams.csv", initialTeamFileContents)

    if (initialTitleFileContents.nonEmpty)
      writeFile("titles.csv", initialTitleFileContents)

    runTest()

    validateFileContents(testName, "members.csv", expectedFileContents)
  }

  cleanup()

  test.get
}

def cleanup(): Unit = {
  deleteFile("members.csv")
  deleteFile("teams.csv")
  deleteFile("titles.csv")
}
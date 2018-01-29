#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

import java.io.{BufferedWriter, File, FileWriter}

import scala.sys.process._


testReadMembers("testGetMember",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2")) { testName =>

  val result = runReadCommand(Seq("./org.scala", "get", "member", "m1"))
  val expected = """{"Member":"m1","Member Foo":"mfoo1"}"""

  validate(testName, expected, result)
}

testReadMembers("testFindMember",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2")) { testName =>

  val result = runReadCommand(Seq("./org.scala", "find", "member", "Member Foo", "mfoo2"))
  val expected = """m2"""

  validate(testName, expected, result)
}

testReadMembers("testFindMemberMultiple",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2")) { testName =>

  val result = runReadCommand(Seq("./org.scala", "find", "member", "Member Foo", "mfoo"))
  val expected = "m1\nm2"

  validate(testName, expected, result)
}

testUpdateMembers("addSimpleMember",
  initialMemberFileContents = Seq("Member (id),Member Foo")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "add", "member", "m1"),
    promptValues = Seq("mfoo")
  )

  Seq("Member (id),Member Foo", "m1,mfoo")
}

testUpdateMembers("addSimpleMemberWithEmpty",
  initialMemberFileContents = Seq("Member (id),Member Foo,Member Bar")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "add", "member", "m1"),
    promptValues = Seq("", "mbar")
  )

  Seq("Member (id),Member Foo,Member Bar", "m1,,mbar")
}

testUpdateMembers("addTeamRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "add", "member", "m1"),
    promptValues = Seq("mfoo", "t1")
  )

  Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t1")
}

testUpdateMembers("addTitleRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Title Ref (title)"),
  initialTitleFileContents = Seq("Title (id),Title Foo", "t1,tfoo")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "add", "member", "m1"),
    promptValues = Seq("mfoo", "t1")
  )

  Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t1")
}

testUpdateMembers("addTeamRefWithPrompt",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo")) { () =>
  
  // An invalid team should prompt to enter one from a numbered list
  runUpdateCommand(
    command = Seq("./org.scala", "add", "member", "m1"),
    promptValues = Seq("mfoo", "t2", "1")
  )

  Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t1")
}

testUpdateMembers("addTitleRefWithPrompt",
  initialMemberFileContents = Seq("Member (id),Member Foo,Title Ref (title)"),
  initialTitleFileContents = Seq("Title (id),Title Foo", "t1,tfoo")) { () =>

  // An invalid title should prompt to enter one from a numbered list
  runUpdateCommand(
    command = Seq("./org.scala", "add", "member", "m1"),
    promptValues = Seq("mfoo", "t2", "1")
  )

  Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t1")
}

testUpdateMembers("updateSimpleMember",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "update", "member", "m1", "Member Foo"),
    promptValues = Seq("mfoo2")
  )

  Seq("Member (id),Member Foo", "m1,mfoo2")
}

testUpdateMembers("updateMemberAddMultiValue",
  initialMemberFileContents = Seq("Member (id),Member Foo (multi)", "m1,mfoo1")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "update", "member", "m1", "add", "Member Foo"),
    promptValues = Seq("mfoo2")
  )

  Seq("Member (id),Member Foo (multi)", "m1,mfoo1|mfoo2")
}

testUpdateMembers("updateMemberRemoveMultiValue",
  initialMemberFileContents = Seq("Member (id),Member Foo (multi)", "m1,mfoo1|mfoo2")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "update", "member", "m1", "remove", "Member Foo"),
    promptValues = Seq("mfoo1")
  )

  Seq("Member (id),Member Foo (multi)", "m1,mfoo2")
}

testUpdateMembers("updateTeamRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t1"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo", "t2,tfoo")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "update", "member", "m1", "Team Ref"),
    promptValues = Seq("t2")
  )

  Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t2")
}

testUpdateMembers("updateTitleRef",
  initialMemberFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t1"),
  initialTitleFileContents = Seq("Title (id),Title Foo", "t1,tfoo", "t2,tfoo")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "update", "member", "m1", "Title Ref"),
    promptValues = Seq("t2")
  )

  Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t2")
}

testUpdateMembers("updateTeamRefWithPrompt",
  initialMemberFileContents = Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t1"),
  initialTeamFileContents = Seq("Team (id),Team Foo", "t1,tfoo", "t2,tfoo")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "update", "member", "m1", "Team Ref"),
    promptValues = Seq("t3", "2")
  )

  Seq("Member (id),Member Foo,Team Ref (team)", "m1,mfoo,t2")
}

testUpdateMembers("updateTitleRefWithPrompt",
  initialMemberFileContents = Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t1"),
  initialTitleFileContents = Seq("Title (id),Title Foo", "t1,tfoo", "t2,tfoo")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "update", "member", "m1", "Title Ref"),
    promptValues = Seq("t3", "2")
  )

  Seq("Member (id),Member Foo,Title Ref (title)", "m1,mfoo,t2")
}

testUpdateMembers("removeMember",
  initialMemberFileContents = Seq("Member (id),Member Foo", "m1,mfoo1", "m2,mfoo2")) { () =>

  runUpdateCommand(
    command = Seq("./org.scala", "remove", "member", "m1")
  )

  Seq("Member (id),Member Foo", "m2,mfoo2")
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
  * Runs a command. STDOUT is sent to /dev/null. STDERR is sent to the console.
  * @param command      The command to run
  * @param promptValues Values to input for the prompts that are expected
  * @return The exit code
  */
def runUpdateCommand(command: Seq[String], promptValues: Seq[String] = Nil): Int = {
  (s"printf ${promptValues.mkString("", """\n""", """\n""")}" #| command #> new File("/dev/null")).!
}

/**
  * Runs a command and returns the output from STDOUT
  * @param command The command to run
  * @return STDOUT
  */
def runReadCommand(command: Seq[String]): String = {
  command.!!.trim
}

// Run a test against a members 'read' command
def testReadMembers(testName: String,
                    initialMemberFileContents: Seq[String],
                    initialTeamFileContents: Seq[String] = Nil,
                    initialTitleFileContents: Seq[String] = Nil)
                   (runTest: String => Unit): Unit = {
  if (initialMemberFileContents.nonEmpty)
    writeFile("members.csv", initialMemberFileContents)

  if (initialTeamFileContents.nonEmpty)
    writeFile("teams.csv", initialTeamFileContents)

  if (initialTitleFileContents.nonEmpty)
    writeFile("titles.csv", initialTitleFileContents)

  runTest(testName)
  cleanup()
}

// Run a test against a members 'write' command. runTest() should return the expected contents of the members.csv file
// after the test has completed.
def testUpdateMembers(testName: String,
                      initialMemberFileContents: Seq[String],
                      initialTeamFileContents: Seq[String] = Nil,
                      initialTitleFileContents: Seq[String] = Nil)
                     (runTest: () => Seq[String]): Unit = {
  if (initialMemberFileContents.nonEmpty)
    writeFile("members.csv", initialMemberFileContents)

  if (initialTeamFileContents.nonEmpty)
    writeFile("teams.csv", initialTeamFileContents)

  if (initialTitleFileContents.nonEmpty)
    writeFile("titles.csv", initialTitleFileContents)

  validateFileContents(testName, "members.csv", runTest())

  cleanup()
}

def cleanup(): Unit = {
  deleteFile("members.csv")
  deleteFile("teams.csv")
  deleteFile("titles.csv")
}
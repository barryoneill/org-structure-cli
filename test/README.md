# test.scala

Because this is a bare bones Scala script project, we don't have things like scalatest
available to us. Yes, we could add a dependency but I wanted to keep things simple.

test.scala runs a serious of functional tests. It does this by creating data files (members.csv,
teams.csv, etc) in some initial state, running a cli command (e.g. ./org.scala add member foo),
verifying the outcome and cleaning up any created files.

The result of each test is printed to the console. If a test fails an exception will be thrown.

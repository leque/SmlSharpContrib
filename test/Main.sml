val suites = SMLUnit.Test.TestList [
  StdTest.suite (),
  SetTest.suite (),
  MapTest.suite (),
  EitherTest.suite (),
  QueueTest.suite (),
  StackTest.suite (),
  SuspTest.suite (),
  LazyListTest.suite (),
  ParserTest.suite (),
  JsonDecoderTest.suite ()
]

val () =
  SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suites

val () =
  OS.Process.exit OS.Process.success

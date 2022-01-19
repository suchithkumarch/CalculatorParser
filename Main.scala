// Calculator Parser in Scala

object Main extends App {
  print(">>")
  val input = scala.io.StdIn.readLine()
  val sample = new Lexer(input)
  val tokenList = sample.tokenize("", List(), input, 0)
  val sampleParser = new Parser(sample)
  val result = sampleParser.solve()
  println(result)
}


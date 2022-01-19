// Calculator Parser in Scala

object Main extends App {
  print(">>")
  val input = scala.io.StdIn.readLine()
  val sample = new Lexer(input)
  val l = sample.tokenize("", List(), input, 0)
  val p = new Parser(sample)
  val res = p.solve()
  println(res)
}


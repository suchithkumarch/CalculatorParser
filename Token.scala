abstract class Token {
  def tokenType: String

  def token: String
}

class MyNumber(inString: String) extends Token {
  override def tokenType: String = "NUM"

  override def token: String = inString
}

class MyOperator(inString: String) extends Token {
  override def token: String = inString

  override def tokenType: String = inString
}



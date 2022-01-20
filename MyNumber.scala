class MyNumber(inString: String) extends Token {
  override def tokenType: String = "NUM"

  override def token: String = inString
}

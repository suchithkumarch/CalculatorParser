class MyOperator(inString: String) extends Token {
  override def token: String = inString

  override def tokenType: String = inString
}

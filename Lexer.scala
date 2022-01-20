// Tokens : '+', -', '/', '*', '(', ')', 'NUM' i.e (0-9)+

class Lexer(val code: String) {
  val tokenList: Array[Token] = tokenize("", List(), code, 0).toArray

  def tokenize(numAcc: String, tokenlist: List[Token], inputStr: String, position: Int): List[Token] = {
    val StrLen = inputStr.length
    position match {
      case StrLen => if (numAcc == "") tokenlist.reverse else (new MyNumber(numAcc) :: tokenlist).reverse
      case _ => {
        val curr = inputStr(position)
        curr match {
          case '+' | '-' | '*' | '/' | '(' | ')' => {
            numAcc match {
              case "" => tokenize("", new MyOperator(inputStr(position).toString) :: tokenlist, inputStr, position + 1)
              case _ => tokenize("", new MyNumber(numAcc) :: tokenlist, inputStr, position)
            }
          }
          case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' => tokenize(numAcc + inputStr(position), tokenlist, inputStr, position + 1)
          case _ => println("Invalid input"); System.exit(0); null
        }
      }
    }
  }
};


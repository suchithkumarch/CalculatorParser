// Tokens : '+', -', '/', '*', '(', ')', 'NUM' i.e (0-9)+

class Lexer(val code: String) {
  val tokenList: Array[Token] = getTokens()

  def getTokens(): Array[Token] = {
    val nums = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    val transitionsNums = nums.map(x => Tuple2(Tuple2(1, x), 2))
    val fsmNum = new FSM(List(1, 2), 1, List(2), transitionsNums, "NUM")
    val ops = List('+', '-', '*', '/', '(', ')')
    val transitionsOps = ops.map(x => Tuple2(Tuple2(1, x), 2))
    val fsmOp = new FSM(List(1, 2), 1, List(2), transitionsOps, "OP")
    val res = tokenize2("", List(), code, 0, fsmOp, fsmNum)
    res.toArray
  }


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

  def checkFSM(fsm: FSM, str: String): Boolean = {
    for (i <- str) {
      fsm.takeInput(i)
    }
    fsm.isAccepted()
  }

  def tokenize2(numAcc: String, tokenlist: List[Token], inputStr: String, position: Int, fsmOp: FSM, fsmNum: FSM): List[Token] = {
    val StrLen = inputStr.length
    position match {
      case StrLen => {
        if (numAcc == "") {
          tokenlist.reverse
        }
        else {
          val n = new MyNumber(numAcc)
          val m = (n :: tokenlist)
          m.reverse
        }
      }
      case _ => {
        val curr = inputStr(position)
        if (checkFSM(fsmNum, curr.toString)) {
          fsmNum.refresh()
          fsmOp.refresh()
          tokenize2(numAcc + inputStr(position), tokenlist, inputStr, position + 1, fsmOp, fsmNum)
        }
        else if (checkFSM(fsmOp, curr.toString)) {
          numAcc match {
            case "" => fsmNum.refresh(); fsmOp.refresh(); tokenize2("", new MyOperator(inputStr(position).toString) :: tokenlist, inputStr, position + 1, fsmOp, fsmNum)
            case _ => fsmNum.refresh(); fsmOp.refresh(); tokenize2("", new MyNumber(numAcc) :: tokenlist, inputStr, position, fsmOp, fsmNum)
          }
        }
        else {
          println("Invalid input")
          System.exit(0)
          null
        }
      }
    }
  }
};
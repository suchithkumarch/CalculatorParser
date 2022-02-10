// Tokens : '+', -', '/', '*', '(', ')', 'NUM' i.e (0-9)+

import scala.annotation.tailrec

class Lexer(val code: String) {
  val tokenList: Array[Token] = getTokens() // tokenize(code).toArray

  def getTokens(): Array[Token] = {
    val nums = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    val transitionsNums = nums.map(x => Tuple2(Tuple2(1, x), 2))
    val fsmNum = new FSM(List(1, 2), 1, List(2), transitionsNums, "NUM")
    val ops = List('+', '-', '*', '/', '(', ')')
    val transitionsOps = ops.map(x => Tuple2(Tuple2(1, x), 2))
    val fsmOp = new FSM(List(1, 2), 1, List(2), transitionsOps, "OP")
    val res = tokenizeFSM(code, fsmOp, fsmNum)
    res.toArray
  }

  def tokenize(inputStr: String): List[Token] = {
    @tailrec def tokenizeLoop(numAcc: String, tokenlist: List[Token], inputStr: String, position: Int): List[Token] = {
      val StrLen = inputStr.length
      position match {
        case StrLen => if (numAcc == "") tokenlist.reverse else (new MyNumber(numAcc) :: tokenlist).reverse
        case _ => {
          val curr = inputStr(position)
          curr match {
            case '+' | '-' | '*' | '/' | '(' | ')' => {
              numAcc match {
                case "" => tokenizeLoop("", new MyOperator(inputStr(position).toString) :: tokenlist, inputStr, position + 1)
                case _ => tokenizeLoop("", new MyNumber(numAcc) :: tokenlist, inputStr, position)
              }
            }
            case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' => tokenizeLoop(numAcc + inputStr(position), tokenlist, inputStr, position + 1)
            case _ => println("Invalid input"); System.exit(0); null
          }
        }
      }
    }

    tokenizeLoop("", List(), inputStr, 0)
  }

  def checkFSM(fsm: FSM, str: String): Boolean = {
    str.foreach(i => fsm.takeInput(i))
    fsm.isAccepted()
  }

  def tokenizeFSM(inStr: String, fsmOp: FSM, fsmNum: FSM): List[Token] = {
    @tailrec def tokenizeFSMLoop(numAcc: String, tokenlist: List[Token], inputStr: String, position: Int, fsmOp: FSM, fsmNum: FSM): List[Token] = {
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
            tokenizeFSMLoop(numAcc + inputStr(position), tokenlist, inputStr, position + 1, fsmOp, fsmNum)
          }
          else if (checkFSM(fsmOp, curr.toString)) {
            numAcc match {
              case "" => fsmNum.refresh(); fsmOp.refresh(); tokenizeFSMLoop("", new MyOperator(inputStr(position).toString) :: tokenlist, inputStr, position + 1, fsmOp, fsmNum)
              case _ => fsmNum.refresh(); fsmOp.refresh(); tokenizeFSMLoop("", new MyNumber(numAcc) :: tokenlist, inputStr, position, fsmOp, fsmNum)
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

    tokenizeFSMLoop("", List(), inStr, 0, fsmOp, fsmNum)
  }
};

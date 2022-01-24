import scala.annotation.tailrec

class FSM(ps: List[Int], iS: Int, fs: List[Int], tr: List[Tuple2[Tuple2[Int, Char], Int]],tp:String) {
  val initialState: Int = iS
  val finalStates: List[Int] = fs
  val possibleStates: List[Int] = ps
  val transitions: List[Tuple2[Tuple2[Int, Char], Int]] = tr
  var currentState:Int=initialState
  val typ:String=tp
  def takeInput(inp: Char): Int = {
    @tailrec def loop(currState: Int, inp: Char, lst: List[Tuple2[Tuple2[Int, Char], Int]]): Int = {
      lst match {
        case Nil => -1
        case ((s, c), s2) :: t => if (s == currState && c == inp) s2 else loop(currState, inp, t)
      }
    }

    val res:Int=loop(currentState,inp, transitions)
    currentState=res
    res
  }

  def isAccepted(): Boolean = {
    finalStates.contains(currentState)
  }

  def isValid(): Boolean = {
    possibleStates.contains(currentState)
  }

  def refresh(): Unit ={
    currentState=initialState
  }
};

/* Calculator grammar:
 *
 * add_expression := multi_expression addHelper
 *
 * addHelper := (('+' | '-') multi_expression addHelper
 *
 * addHelper := EOF
 *
 * multi_expression := atomic_expression multiHelper
 *
 * multiHelper := (('*' | '/') atomic_expression multiHelper
 *
 * multiHelper := EOF
 *
 * atomic_expression := number | left_parenthesis add_expression right_parenthesis
 *
 * number := NUM
 *
 */

class Parser(tok: Lexer) {
  var position: Int = 0;

  def solve(): Double = {
    calculate(parse_add_expression())
  }

  def calculate(t: Tree): Double = {
    t.typ match {
      case "NUM" => t.value
      case "+" => calculate(t.left) + calculate(t.right)
      case "-" => calculate(t.left) - calculate(t.right)
      case "*" => calculate(t.left) * calculate(t.right)
      case "/" => {
        val l = calculate(t.left);
        val r = calculate(t.right);
        r match {
          case 0 => {
            println("Division by 0 error");
            System.exit(0);
            0.0
          }
          case _ => l / r
        }
      }
      case _ => 0.0
    }
  }

  def addHelper(leftTree: Tree): Tree = {
    val cond: Boolean = position < tok.tokens.length && (tok.tTypes(position) == "+" || tok.tTypes(position) == "-")
    cond match {
      case true => {
        position += 1
        val tp = tok.tTypes(position - 1)
        val rightTree = parse_multi_expression();
        val tre = new Tree(tp, 0, leftTree, rightTree)
        addHelper(tre);
      }
      case _ => {
        leftTree
      }
    }
  }

  def parse_add_expression(): Tree = {
    val leftTree = parse_multi_expression();
    addHelper(leftTree)
  }

  def multiHelper(leftTree: Tree): Tree = {
    val cond: Boolean = position < tok.tokens.length && (tok.tTypes(position) == "*" || tok.tTypes(position) == "/")
    cond match {
      case true => {
        position += 1
        val tp = tok.tTypes(position - 1)
        val rightTree = parse_atomic_expression();
        val tre = new Tree(tp, 0, leftTree, rightTree)
        multiHelper(tre);
      }
      case _ => {
        leftTree
      }
    }
  }

  def parse_multi_expression(): Tree = {
    val leftTree = parse_atomic_expression();
    multiHelper(leftTree)
  }

  def parse_atomic_expression(): Tree = {
    if (position < tok.tokens.length) {
      tok.tTypes(position) match {
        case "(" => {
          position += 1
          val exp = parse_add_expression()
          tok.tTypes(position) match {
            case ")" => {
              position += 1
              exp
            }
            case _ => {
              println("Invalid input");
              System.exit(0)
              null
            }
          }
        }
        case _ => {
          parse_number();
        }
      }
    }
    else {
      println("Invalid input");
      System.exit(0)
      null
    }
  }


  def parse_number(): Tree = {
    if (position < tok.tokens.length) {
      tok.tTypes(position) match {
        case "NUM" => {
          position += 1
          var t = new Tree("NUM", tok.tokens(position - 1).toDouble, null, null)
          t;
        }
        case _ => {
          println("Invalid input");
          System.exit(0)
          null
        }
      }
    }
    else {
      println("Invalid input");
      System.exit(0)
      null
    }
  }
};

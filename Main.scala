// Calculator Parser in Scala by Suchith

class Tree( typ1:String, value1:Double,left1:Tree,right1:Tree){
  val typ:String=typ1;
  var value:Double=value1;
  var left:Tree=left1;
  var right:Tree=right1;
};

// Tokens : '+', -', '/', '*', '(', ')', 'NUM' i.e (0-9)+

class Tokens(var code:String){
  val symbols=List('+','-','/','*','(',')');
  val num=List('1','2','3','4','5','6','7','8','9','0');
  var tokens:  Array[String]=new Array[String](1+code.length);
  var tTypes:  Array[String]=new Array[String](1+code.length);

  def tokenize(currNum:String, i:Int , j:Int){
    if(i>=code.length ){
      if (currNum.equals("")){

      }
      else{
        tokens(j)=currNum
        tTypes(j)="NUM"
      }
    }
    else if(symbols.contains(code(i))){
      if ( ! currNum.equals("")  ){
        tokens(j)=currNum
        tTypes(j)="NUM"
        tokenize("",i,j+1);
      }
      else{
        tokens(j)=code(i).toString
        tTypes(j)=code(i).toString
        tokenize(currNum,i+1,j+1)
      }
    }
    else if(num.contains(code(i))){
      tokenize(currNum + code(i) , i+1 , j)
    }
  }
};

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

class Parser(tok: Tokens){
  var position : Int = 0;
  def solve(): Double ={
    calculate(parse_add_expression())
  }
  def calculate(t:Tree):Double= {
    t.typ match {
      case "NUM" => t.value
      case "+" => calculate(t.left) + calculate(t.right)
      case "-" => calculate(t.left) - calculate(t.right)
      case "*" => calculate(t.left) * calculate(t.right)
      case "/" => {
        val l = calculate(t.left);
        val r = calculate(t.right);
        r match {
          case 0 => {println("Division by 0 error"); System.exit(0); 0.0}
          case _ => l / r
        }
      }
      case _ => 0.0
    }
  }
  def addHelper(leftTree:Tree):Tree={
    val cond:Boolean= position<tok.tokens.length && (tok.tTypes(position)=="+" || tok.tTypes(position)=="-")
    cond match {
      case true => {
        position+=1
        val tp=tok.tTypes(position-1)
        val rightTree=parse_multi_expression();
        val tre = new Tree(tp,0,leftTree,rightTree)
        addHelper(tre);
      }
      case _ => {
        leftTree
      }
    }
  }
  def parse_add_expression(): Tree ={
      val leftTree=parse_multi_expression();
      addHelper(leftTree)
  }

  def multiHelper(leftTree:Tree): Tree= {
    val cond: Boolean = position < tok.tokens.length && (tok.tTypes(position) == "*" || tok.tTypes(position) == "/")
    cond match {
      case true => {
        position += 1
        val tp=tok.tTypes(position-1)
        val rightTree=parse_atomic_expression();
        val tre = new Tree(tp,0,leftTree,rightTree)
        multiHelper(tre);
      }
      case _ => {leftTree}
    }
  }

  def parse_multi_expression(): Tree = {
      val leftTree=parse_atomic_expression();
      multiHelper(leftTree)
  }

  def parse_atomic_expression(): Tree = {
      tok.tTypes(position) match {
        case "(" => {
          position += 1
          val exp=parse_add_expression()
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

  def parse_number(): Tree = {
      tok.tTypes(position) match {
        case "NUM" => {
          position += 1
          var t=new Tree("NUM",tok.tokens(position-1).toDouble,null,null)
          t;
        }
        case _ => {
          println("Invalid input");
          System.exit(0)
          null
        }
      }
  }
};

object Main extends App {
  print(">>")
  val input = scala.io.StdIn.readLine()
  val sample = new Tokens(input);
  sample.tokenize("",0,0);
  val p= new Parser(sample);
  val res=p.solve()
  println(res)
};

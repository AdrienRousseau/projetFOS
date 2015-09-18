package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero","2")

  import lexical.NumericLit
  import lexical.StringLit

  /** term ::= 'true'
             | 'false'
             | 'if' term 'then' term 'else' term
             | '0'
             | 'succ' term
             | 'pred' term
             | 'iszero' term
   */
  def term: Parser[Term] = ("true" ^^^ True 
      | "false" ^^^ False 
      | "0" ^^^ Zero 
      |  "if" ~> term ~ "then" ~ term ~ "else" ~ term ^^ { case t1 ~ "then" ~ t2 ~ "else" ~ t3 => If(t1,t2,t3)}
      | "succ" ~> term ^^ ((t: Term) => Succ(t)) 
      | "pred" ~> term ^^ ((t: Term) => Pred(t))
      | "iszero" ~> term ^^((t: Term) => IsZero(t))
      | numericLit ^^ ( num => ConvertInt(num.toString().toInt)) );
    
    // numericLit ^^ ((num: NumericLit) => ConvertInt(num.toString().toInt())))
    
  def ConvertInt(num: Int) : Term =
    if (num == 0) {
      Zero
    }
    else {
      Succ(ConvertInt(num -1))
    } ;
  
  
  
  
  case class NoReductionPossible(t: Term) extends Exception(t.toString)

  /** Return a list of at most n terms, each being one step of reduction. */
  def path(t: Term, n: Int = 64): List[Term] =
    if (n <= 0) Nil
    else
      t :: {
        try {
          path(reduce(t), n - 1)
        } catch {
          case NoReductionPossible(t1) =>
            Nil
        }
      }

  /** Perform one step of reduction, when possible.
   *  If reduction is not possible NoReductionPossible exception
   *  with corresponding irreducible term should be thrown.
   */
  def reduce(t: Term): Term =
    ???

  case class TermIsStuck(t: Term) extends Exception(t.toString)

  /** Perform big step evaluation (result is always a value.)
   *  If evaluation is not possible TermIsStuck exception with
   *  corresponding inner irreducible term should be thrown.
   */
  def eval(t: Term): Term =
    ???

  def main(args: Array[String]): Unit = {
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(term)(tokens) match {
      case Success(trees, _) =>
        for (t <- path(trees))
          println(t)
        try {
          print("Big step: ")
          println(eval(trees))
        } catch {
          case TermIsStuck(t) => println("Stuck term: " + t)
        }
      case e =>
        println(e)
    }
  }
}

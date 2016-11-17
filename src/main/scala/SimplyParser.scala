/**
  * Created by Benjamin Byholm on 16.11.2016.
  */
import java.io.FileReader

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

class SimplyParser extends RegexParsers with PackratParsers {
  def simply_problem: Parser[Any] = "Problem" ~ ":" ~ id ~ data ~ domains ~ variables ~ constraints

  def data: Parser[Any] = "Data" ~ rep(data_exp)

  def data_exp: Parser[Any] = (
    id ~ ":=" ~ arithm_exp ~ ";"
  | id ~ ":=" ~ formula ~ ";"
  )

  def domains: Parser[Any] = "Domains" ~ rep(domain_exp)

  def domain_exp: Parser[Any] = "Dom" ~ id ~ "=" ~ list ~ ";"

  def variables: Parser[Any] = "Variables" ~ rep(variable_exp)

  def variable_exp: Parser[Any] = (
    "IntVar" ~ rep1sep(var_id, ",") ~ "::" ~ id ~ ";"
  | "BoolVar" ~ rep1sep(var_id, ",") ~ ";"
  )

  def var_id: Parser[Any] = (
    id ~ "[" ~ rep1sep(arithm_exp, ",") ~ "]"
  | id
  )

  def constraints: Parser[Any] = "Constraints" ~ rep1(sentence)

  def sentence: Parser[Any] = (
    statement
  | constraint ~ ";"
  )

  def statement: Parser[Any] = (
    if_then_else
  | forall
  )

  def if_then_else: Parser[Any] = (
    "If" ~ "(" ~ formula ~ ")" ~ "Then" ~ "{" ~ rep1(sentence) ~ "}"
  | "If" ~ "(" ~ formula ~ ")" ~ "Then" ~ "{" ~ rep1(sentence) ~ "}" ~ "Else" ~ "{" ~ rep1(sentence) ~ "}"
  )

  def forall: Parser[Any] = "Forall" ~ "(" ~ id ~ "in" ~ list ~ ")" ~ "{" ~ rep1(sentence) ~ "}"

  def constraint: Parser[Any] = (
    global_constraint
  | "If_Then_Else" ~ "(" ~ formula ~ ")" ~ "{" ~ rep1(sentence) ~ "}" ~ "{" ~ rep1(sentence) ~ "}"
  | formula
  )

  lazy val formula: PackratParser[Any] = (
    "Not" ~ formula
  | formula ~ bool_operator ~ formula
  | arithm_exp ~ relational_operator ~ arithm_exp
  | "(" ~ formula ~ ")"
  | var_id
  | "True"
  | "False"
  )

  def global_constraint: Parser[Any] = (
    "AllDifferent" ~ "(" ~ list ~ ")"
  | "Sum" ~ "(" ~ list ~ "," ~ arithm_exp ~ ")"
  | "Count" ~ "(" ~ list ~ "," ~ arithm_exp ~ "," ~ arithm_exp ~ ")"
  )

  lazy val arithm_exp: PackratParser[Any] = (
    arithm_exp ~ arithm_operator ~ arithm_exp
  | numeral
  | var_id
  | "(" ~ arithm_exp ~ ")"
  | "Abs" ~ "(" ~ arithm_exp ~ ")"
  )

  def list: Parser[Any] = (
    "[" ~ rep1sep(list_element, ",") ~ "]"
  | "[" ~ arithm_exp ~ "|" ~ rep1sep(var_restrict, ",") ~ "]"
  )

  def list_element: Parser[Any] = (
    range
  | arithm_exp
  )

  def var_restrict: Parser[Any] = (
    id ~ "in" ~ list
  | formula
  )

   def range: Parser[Any] = arithm_exp~".."~arithm_exp

  def bool_operator: Parser[Any] = (
    "And"
  | "Or"
  | "Xor"
  | "Iff"
  | "Implies"
  )

  def relational_operator: Parser[Any] = (
    "="
  | "<>"
  | "<"
  | ">"
  | "=<"
  | ">="
  )

  def arithm_operator: Parser[Any] = (
    "+"
  | "-"
  | "*"
  | "Div"
  | "Mod"
  )

  def id: Parser[String] = """[A-Za-z_]\w*""".r ^^ { _.toString }

  def numeral: Parser[Int] = """\d+""".r ^^ { _.toInt }
}

object SimplyParserTest extends SimplyParser {
  def main(args: Array[String]): Unit = {
    val reader = new FileReader("resources/SchursLemma_10_3.y")
    println(parseAll(simply_problem, reader))
    val reader_2 = new FileReader("resources/queens_8.y")
    println(parseAll(simply_problem, reader_2))
  }
}

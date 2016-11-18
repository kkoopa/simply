/**
  * Created by Benjamin Byholm on 18.11.2016.
  */
import java.io.FileReader

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

class SimplyParser extends RegexParsers with PackratParsers {
  override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def simply_problem: Parser[PROBLEM] = "Problem" ~> ":" ~> id ~ data ~ domains ~ variables ~ constraints ^^ { case ident ~ da ~ dom ~ va ~ cons => PROBLEM(ident, da, dom, va, cons) }

  def data: Parser[List[DATA_EXP]] = "Data" ~> rep(data_exp)

  def data_exp: Parser[DATA_EXP] = (
    id ~ ":=" ~ arithm_exp <~ ";" ^^ { case ident ~ ":=" ~ exp => DATA_EXP(ident, exp) }
  | id ~ ":=" ~ formula <~ ";" ^^ { case ident ~ ":=" ~ predicate => DATA_EXP(ident, predicate)}
  )

  def domains: Parser[List[DOMAIN_EXP]] = "Domains" ~> rep(domain_exp)

  def domain_exp: Parser[DOMAIN_EXP] = "Dom" ~> id ~ "=" ~ list <~ ";" ^^ { case ident ~ "=" ~ l => DOMAIN_EXP(ident, l) }

  def variables: Parser[List[VARIABLE_EXP]] = "Variables" ~> rep(variable_exp)

  def variable_exp: Parser[VARIABLE_EXP] = (
    "IntVar" ~> rep1sep(var_id, ",") ~ "::" ~ id <~ ";" ^^ { case vars ~ "::" ~ ident => VARIABLE_EXP(vars, ident) }
  | "BoolVar" ~> rep1sep(var_id, ",") <~ ";" ^^ { x => VARIABLE_EXP(x, IDENTIFIER("__BOOL__"))}
  )

  def var_id: Parser[VAR_ID] = (
    id ~ "[" ~ rep1sep(arithm_exp, ",") <~ "]" ^^ { case id ~ "[" ~ (list : List[ARITHM_EXP]) => VAR_ID(id, list)}
  | id ^^ { x => VAR_ID(x) }
  )

  def constraints: Parser[List[SENTENCE]] = "Constraints" ~> rep1(sentence)

  def sentence: Parser[SENTENCE] = (
    statement ^^ { x => SENTENCE_STATEMENT(x) }
  | constraint <~ ";" ^^ { x => SENTENCE_CONSTRAINT(x) }
  )

  def statement: Parser[STATEMENT] = (
    if_then_else ^^ { x => IF_THEN_ELSE_STATEMENT(x) }
  | forall ^^ { x => FORALL_STATEMENT(x) }
  )

  def if_then_else: Parser[IF_THEN_ELSE] = (
    "If" ~> "(" ~> formula ~ ")" ~ "Then" ~ "{" ~ rep1(sentence) <~ "}" ^^ { case f ~ ")" ~ "Then" ~ "{" ~ ifs => IF_THEN_ELSE(f, ifs) }
  | "If" ~> "(" ~> formula ~ ")" ~ "Then" ~ "{" ~ rep1(sentence) ~ "}" ~ "Else" ~ "{" ~ rep1(sentence) <~ "}" ^^ { case f ~ ")" ~ "Then" ~ "{" ~ ifs ~ "}" ~ "Else" ~ "{" ~ elses => IF_THEN_ELSE(f, ifs, elses) }
  )

  def forall: Parser[FORALL] = "Forall" ~> "(" ~> id ~ "in" ~ list ~ ")" ~ "{" ~ rep1(sentence) <~ "}" ^^ { case ident ~ "in" ~ l ~ ")" ~ "{" ~ sentences => FORALL(ident, l, sentences) }

  def constraint: Parser[CONSTRAINT] = (
    global_constraint
  | "If_Then_Else" ~> "(" ~> formula ~ ")" ~ "{" ~ rep1(sentence) ~ "}" ~ "{" ~ rep1(sentence) <~ "}" ^^ { case f ~ ")" ~ "{" ~ ifs ~ "}" ~ "{" ~ elses => IF_THEN_ELSE_CONSTRAINT(f, ifs, elses) }
  | formula ^^ { x => PREDICATE_CONSTRAINT(x) }
  )

  lazy val formula: PackratParser[FORMULA] = (
    "Not" ~> formula ^^ { x => NOT_FORMULA(x) }
  | formula ~ bool_operator ~ formula ^^ { case lhs ~ op ~ rhs => BOOL_OP_FORMULA(op, lhs, rhs) }
  | arithm_exp ~ relational_operator ~ arithm_exp ^^ { case lhs ~ op ~ rhs  => REL_OP_FORMULA(op, lhs, rhs) }
  | "(" ~> formula <~ ")"
  | var_id ^^ { x => VAR_FORMULA(x) }
  | "True" ^^^ { CONST_FORMULA(true) }
  | "False" ^^^ { CONST_FORMULA(false) }
  )

  def global_constraint: Parser[GLOBAL_CONSTRAINT] = (
    "AllDifferent" ~> "(" ~> list <~ ")" ^^ { x => ALLDIFFERENT_CONSTRAINT(x) }
  | "Sum" ~> "(" ~> list ~ "," ~ arithm_exp <~ ")" ^^ { case l ~ "," ~ exp => SUM_CONSTRAINT(l, exp) }
  | "Count" ~> "(" ~> list ~ "," ~ arithm_exp ~ "," ~ arithm_exp <~ ")" ^^ { case l ~ "," ~ value ~ "," ~ count => COUNT_CONSTRAINT(l, value, count) }
  )

  lazy val arithm_exp: PackratParser[ARITHM_EXP] = (
    arithm_exp ~ arithm_operator ~ arithm_exp ^^ { case lhs ~ op ~ rhs => ARITHM_OP_EXP(op, lhs, rhs) }
    | "Abs" ~> "(" ~> arithm_exp <~ ")" ^^ { x => ABS_EXP(x) }
    | numeral ^^ { x => CONST_EXP(x) }
    | var_id ^^ { x => VAR_EXP(x) }
    | "(" ~> arithm_exp <~ ")"
  )

  def list: Parser[LIST] = (
    "[" ~> rep1sep(list_element, ",") <~ "]" ^^ { x => LIST_ENUMERATION(x) }
  | "[" ~> arithm_exp ~ "|" ~ rep1sep(var_restrict, ",") <~ "]" ^^ { case exp ~ "|" ~ r => LIST_COMPREHENSION(exp, r) }
  )

  def list_element: Parser[LIST_ELEMENT] = (
    range ^^ { x => LIST_ELEMENT_RANGE(x) }
  | arithm_exp ^^ { x => LIST_ELEMENT_EXP(x) }
  )

  def var_restrict: Parser[VAR_RESTRICT] = (
    id ~ "in" ~ list ^^ { case ident ~ "in" ~ l => MEMBER_RESTRICT(ident, l) }
  | formula ^^ { x => PREDICATE_RESTRICT(x) }
  )

  def range: Parser[RANGE] = arithm_exp ~ ".." ~ arithm_exp ^^ { case lb ~ ".." ~ ub => RANGE(lb, ub) }

  def bool_operator: Parser[String] = (
    "And"
  | "Or"
  | "Xor"
  | "Iff"
  | "Implies"
  )

  def relational_operator: Parser[String] = (
    "=<"
  | "="
  | "<>"
  | "<"
  | ">="
  | ">"
  )

  def arithm_operator: Parser[String] = (
    "+"
  | "-"
  | "*"
  | "Div"
  | "Mod"
  )

  def id: Parser[IDENTIFIER] = """[A-Za-z_]\w*""".r ^^ { x => IDENTIFIER(x.toString)}

  def numeral: Parser[NUMERAL] = """\d+""".r ^^ { x => NUMERAL(x.toInt) }
}

object SimplyParserTest extends SimplyParser {
  def main(args: Array[String]): Unit = {
    val reader = new FileReader("target/scala-2.12/classes/SchursLemma_10_3.y")
    println(parseAll(simply_problem, reader))
    val reader_2 = new FileReader("target/scala-2.12/classes/queens_8.y")
    println(parseAll(simply_problem, reader_2))
    val reader_3 = new FileReader("target/scala-2.12/classes/bacp_12_6.y")
    println(parseAll(simply_problem, reader_3))
    val reader_4 = new FileReader("target/scala-2.12/classes/jobshop_58.y")
    println(parseAll(simply_problem, reader_4))
  }
}

/**
  * Created by Benjamin Byholm on 18.11.2016.
  */
import java.io.FileReader

import scala.util.parsing.combinator.RegexParsers

import Printer.AnyTreeString

class SimplyParser extends RegexParsers {
  override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def simply_problem: Parser[PROBLEM] = "Problem" ~> ":" ~> id ~ data ~ domains ~ variables ~ constraints ^^ { case ident ~ da ~ dom ~ va ~ cons => PROBLEM(ident, da, dom, va, cons) }

  def data: Parser[List[DATA_EXP]] = "Data" ~> rep(data_exp)

  def data_exp: Parser[DATA_EXP] = (
    id ~ (":=" ~> arithm_exp) <~ ";" ^^ { case ident ~ exp => DATA_EXP(ident, exp) }
  | id ~ (":=" ~> formula) <~ ";" ^^ { case ident ~ predicate => DATA_EXP(ident, predicate)}
  )

  def domains: Parser[List[DOMAIN_EXP]] = "Domains" ~> rep(domain_exp)

  def domain_exp: Parser[DOMAIN_EXP] = "Dom" ~> id ~ ("=" ~> list) <~ ";" ^^ { case ident ~ l => DOMAIN_EXP(ident, l) }

  def variables: Parser[List[VARIABLE_EXP]] = "Variables" ~> rep(variable_exp)

  def variable_exp: Parser[VARIABLE_EXP] = (
    "IntVar" ~> rep1sep(var_id, ",") ~ ("::" ~> id) <~ ";" ^^ { case vars ~ ident => VARIABLE_EXP(vars, ident) }
  | "BoolVar" ~> rep1sep(var_id, ",") <~ ";" ^^ { VARIABLE_EXP(_, IDENTIFIER("__BOOL__"))}
  )

  def var_id: Parser[VAR_ID] = (
    id ~ ("[" ~> rep1sep(arithm_exp, ",")) <~ "]" ^^ { case id ~ list => VAR_ID(id, list)}
  | id ^^ { VAR_ID(_) }
  )

  def constraints: Parser[List[SENTENCE]] = "Constraints" ~> rep1(sentence)

  def sentence: Parser[SENTENCE] = (
    statement ^^ { SENTENCE_STATEMENT }
  | constraint <~ ";" ^^ { SENTENCE_CONSTRAINT }
  )

  def statement: Parser[STATEMENT] = (
    if_then_else ^^ { IF_THEN_ELSE_STATEMENT }
  | forall ^^ { FORALL_STATEMENT }
  )

  def if_then_else: Parser[IF_THEN_ELSE] = (
    "If" ~> "(" ~> formula ~ (")" ~> "Then" ~> "{" ~> rep1(sentence)) <~ "}" ^^ { case f ~ ifs => IF_THEN_ELSE(f, ifs) }
  | "If" ~> "(" ~> formula ~ (")" ~> "Then" ~> "{" ~> rep1(sentence)) ~ ("}" ~> "Else" ~> "{" ~> rep1(sentence)) <~ "}" ^^ { case f ~ ifs ~ elses => IF_THEN_ELSE(f, ifs, elses) }
  )

  def forall: Parser[FORALL] = "Forall" ~> "(" ~> id ~ ("in" ~> list) ~ (")" ~> "{" ~> rep1(sentence)) <~ "}" ^^ { case ident ~ l ~ sentences => FORALL(ident, l, sentences) }

  def constraint: Parser[CONSTRAINT] = (
    global_constraint
  | "If_Then_Else" ~> "(" ~> formula ~ (")" ~> "{" ~> rep1(sentence)) ~ ("}" ~> "{" ~> rep1(sentence)) <~ "}" ^^ { case f ~ ifs ~ elses => IF_THEN_ELSE_CONSTRAINT(f, ifs, elses) }
  | formula ^^ { PREDICATE_CONSTRAINT }
  )

  def formula: Parser[FORMULA] = (
    "Not" ~> formula ~ bool_operator ~ formula ^^ { case lhs ~ op ~ rhs => BOOL_OP_FORMULA(op, NOT_FORMULA(lhs), rhs) }
  | "Not" ~> formula ^^ { NOT_FORMULA }
  | arithm_exp ~ relational_operator ~ arithm_exp ~ bool_operator ~ formula ^^ { case lhs1 ~ op1 ~ rhs1 ~ op ~ rhs  => BOOL_OP_FORMULA(op, REL_OP_FORMULA(op1, lhs1, rhs1), rhs) }
  | arithm_exp ~ relational_operator ~ arithm_exp ^^ { case lhs ~ op ~ rhs  => REL_OP_FORMULA(op, lhs, rhs) }
  | "(" ~> formula ~ (")" ~> bool_operator) ~ formula ^^ { case lhs ~ op ~ rhs => BOOL_OP_FORMULA(op, lhs, rhs) }
  | "(" ~> formula <~ ")"
  | var_id ~ bool_operator ~ formula ^^ { case lhs ~ op ~ rhs => BOOL_OP_FORMULA(op, VAR_FORMULA(lhs), rhs) }
  | var_id ^^ { VAR_FORMULA }
  | "True" ~ bool_operator ~ formula ^^ { case lhs ~ op ~ rhs => BOOL_OP_FORMULA(op, CONST_FORMULA(true), rhs) }
  | "True" ^^^ { CONST_FORMULA(true) }
  | "False" ~ bool_operator ~ formula ^^ { case lhs ~ op ~ rhs => BOOL_OP_FORMULA(op, CONST_FORMULA(false), rhs) }
  | "False" ^^^ { CONST_FORMULA(false) }
  )

  def global_constraint: Parser[GLOBAL_CONSTRAINT] = (
    "AllDifferent" ~> "(" ~> list <~ ")" ^^ { ALLDIFFERENT_CONSTRAINT }
  | "Sum" ~> "(" ~> list ~ ("," ~> arithm_exp) <~ ")" ^^ { case l ~ exp => SUM_CONSTRAINT(l, exp) }
  | "Count" ~> "(" ~> list ~ ("," ~> arithm_exp) ~ ("," ~> arithm_exp) <~ ")" ^^ { case l ~ value ~ count => COUNT_CONSTRAINT(l, value, count) }
  )

  def arithm_exp: Parser[ARITHM_EXP] = (
      "Abs" ~> "(" ~> arithm_exp ~ (")" ~> arithm_operator) ~ arithm_exp  ^^ { case lhs ~ op ~ rhs => ARITHM_OP_EXP(op, ABS_EXP(lhs), rhs) }
    | "Abs" ~> "(" ~> arithm_exp <~ ")" ^^ { ABS_EXP }
    | numeral ~ arithm_operator ~ arithm_exp ^^ { case lhs ~ op ~ rhs => ARITHM_OP_EXP(op, CONST_EXP(lhs), rhs) }
    | numeral ^^ { CONST_EXP }
    | var_id ~ arithm_operator ~ arithm_exp ^^ { case lhs ~ op ~ rhs => ARITHM_OP_EXP(op, VAR_EXP(lhs), rhs) }
    | var_id ^^ { VAR_EXP }
    | "(" ~> arithm_exp ~ (")" ~> arithm_operator) ~ arithm_exp ^^ { case lhs ~ op ~ rhs => ARITHM_OP_EXP(op, lhs, rhs) }
    | "(" ~> arithm_exp <~ ")"
  )

  def list: Parser[LIST] = (
    "[" ~> rep1sep(list_element, ",") <~ "]" ^^ { LIST_ENUMERATION }
  | "[" ~> arithm_exp ~ ("|" ~> rep1sep(var_restrict, ",")) <~ "]" ^^ { case exp ~ r => LIST_COMPREHENSION(exp, r) }
  )

  def list_element: Parser[LIST_ELEMENT] = (
    range ^^ { LIST_ELEMENT_RANGE }
  | arithm_exp ^^ { LIST_ELEMENT_EXP }
  )

  def var_restrict: Parser[VAR_RESTRICT] = (
    id ~ ("in" ~> list) ^^ { case ident ~ l => MEMBER_RESTRICT(ident, l) }
  | formula ^^ { PREDICATE_RESTRICT }
  )

  def range: Parser[RANGE] = arithm_exp ~ (".." ~> arithm_exp) ^^ { case lb ~ ub => RANGE(lb, ub) }

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

  def id: Parser[IDENTIFIER] = """[A-Za-z_]\w*""".r ^^ { IDENTIFIER }

  def numeral: Parser[NUMERAL] = """\d+""".r ^^ { string => NUMERAL(string.toInt) }
}

object SimplyParserTest extends SimplyParser {
  def main(args: Array[String]): Unit = {
    val reader = new FileReader("target/scala-2.12/classes/SchursLemma_10_3.y")
    println(parseAll(simply_problem, reader).get.treeString)
    val reader_2 = new FileReader("target/scala-2.12/classes/queens_8.y")
    println(parseAll(simply_problem, reader_2).get.treeString)
    val reader_3 = new FileReader("target/scala-2.12/classes/bacp_12_6.y")
    println(parseAll(simply_problem, reader_3).get.treeString)
    val reader_4 = new FileReader("target/scala-2.12/classes/jobshop_58.y")
    println(parseAll(simply_problem, reader_4).get.treeString)
  }
}

/**
  * Created by Benjamin Byholm on 18.11.2016.
  */

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class Parser extends RegexParsers {
  override val whiteSpace : Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def simply_problem: Parser[PROBLEM] = "Problem" ~> ":" ~> id ~ data ~ domains ~ variables ~ constraints ^^ { case ident ~ da ~ dom ~ va ~ cons => PROBLEM(ident, da, dom, va, cons) }

  def data: Parser[List[DATA_EXP]] = "Data" ~> data_exp.*

  def data_exp: Parser[DATA_EXP] = (
    id ~ (":=" ~> arithm_exp) <~ ";" ^^ { case ident ~ exp => DATA_EXP(ident, exp) }
  | id ~ (":=" ~> formula) <~ ";" ^^ { case ident ~ predicate => DATA_EXP(ident, predicate)}
  )

  def domains: Parser[List[DOMAIN_EXP]] = "Domains" ~> domain_exp.*

  def domain_exp: Parser[DOMAIN_EXP] = "Dom" ~> id ~ ("=" ~> list) <~ ";" ^^ { case ident ~ l => DOMAIN_EXP(ident, l) }

  def variables: Parser[List[VARIABLE_EXP]] = "Variables" ~> variable_exp.*

  def variable_exp: Parser[VARIABLE_EXP] = (
    "IntVar" ~> rep1sep(var_id, ",") ~ ("::" ~> id) <~ ";" ^^ { case vars ~ ident => VARIABLE_EXP(vars, ident) }
  | "BoolVar" ~> rep1sep(var_id, ",") <~ ";" ^^ { VARIABLE_EXP(_, IDENTIFIER("__BOOL__"))}
  )

  def var_id: Parser[VAR_ID] = (
    id ~ ("[" ~> rep1sep(arithm_exp, ",")) <~ "]" ^^ { case id ~ list => VAR_ID(id, list)}
  | id ^^ { VAR_ID(_) }
  )

  def constraints: Parser[List[SENTENCE]] = "Constraints" ~> sentence.+

  def sentence: Parser[SENTENCE] = (
    statement ^^ { SENTENCE_STATEMENT }
  | constraint <~ ";" ^^ { SENTENCE_CONSTRAINT }
  )

  def statement: Parser[STATEMENT] = (
    if_then_else ^^ { IF_THEN_ELSE_STATEMENT }
  | forall ^^ { FORALL_STATEMENT }
  )

  def if_then_else: Parser[IF_THEN_ELSE] = (
    "If" ~> "(" ~> formula ~ (")" ~> "Then" ~> "{" ~> sentence.+) <~ "}" ^^ { case f ~ ifs => IF_THEN_ELSE(f, ifs) }
  | "If" ~> "(" ~> formula ~ (")" ~> "Then" ~> "{" ~> sentence.+) ~ ("}" ~> "Else" ~> "{" ~> sentence.+) <~ "}" ^^ { case f ~ ifs ~ elses => IF_THEN_ELSE(f, ifs, elses) }
  )

  def forall: Parser[FORALL] = "Forall" ~> "(" ~> id ~ ("in" ~> list) ~ (")" ~> "{" ~> sentence.+) <~ "}" ^^ { case ident ~ l ~ sentences => FORALL(ident, l, sentences) }

  def constraint: Parser[CONSTRAINT] = (
    global_constraint
  | "If_Then_Else" ~> "(" ~> formula ~ (")" ~> "{" ~> sentence.+) ~ ("}" ~> "{" ~> sentence.+) <~ "}" ^^ { case f ~ ifs ~ elses => IF_THEN_ELSE_CONSTRAINT(f, ifs, elses) }
  | formula ^^ { PREDICATE_CONSTRAINT }
  )

  def formula: Parser[FORMULA] = equivalence

  def relation: Parser[FORMULA] = (
    equality
  | inequality
  )

  def equality: Parser[FORMULA] = arithm_exp ~ relational_equality_operator ~ arithm_exp ^^ { case lhs ~ op ~ rhs => REL_OP_FORMULA(op, lhs, rhs) }
  def inequality: Parser[FORMULA] = arithm_exp ~ relational_inequality_operator ~ arithm_exp ^^ { case lhs ~ op ~ rhs => REL_OP_FORMULA(op, lhs, rhs) }

  def equivalence: Parser[FORMULA] = implication ~ ("Iff" ~> implication).* ^^ { case head ~ tail => (head /: tail)((lhs, rhs) => BOOL_OP_FORMULA("Iff", lhs, rhs))}
  def implication: Parser[FORMULA] = disjunction ~ ("Implies" ~> disjunction).* ^^ { case head ~ tail => (head /: tail)((lhs, rhs) => BOOL_OP_FORMULA("Implies", lhs, rhs)) }
  def disjunction: Parser[FORMULA] = exdisjunction ~ ("Or" ~> exdisjunction).* ^^ { case head ~ tail => (head /: tail)((lhs, rhs) => BOOL_OP_FORMULA("Or", lhs, rhs)) }
  def exdisjunction: Parser[FORMULA] = conjunction ~ ("Xor" ~> conjunction).* ^^ { case head ~ tail => (head /: tail)((lhs, rhs) => BOOL_OP_FORMULA("Xor", lhs, rhs)) }
  def conjunction: Parser[FORMULA] = logical_primitive ~ ("And" ~> logical_primitive).* ^^ { case head ~ tail => (head /: tail)((lhs, rhs) => BOOL_OP_FORMULA("And", lhs, rhs)) }

  def logical_primitive: Parser[FORMULA] = (
    "(" ~> formula <~ ")"
  | "Not" ~> formula ^^ { NOT_FORMULA }
  | relation
  | "True" ^^^ { CONST_FORMULA(true) }
  | "False" ^^^ { CONST_FORMULA(false) }
  | var_id ^^ { VAR_FORMULA }
  )

  def global_constraint: Parser[GLOBAL_CONSTRAINT] = (
    "AllDifferent" ~> "(" ~> list <~ ")" ^^ { ALLDIFFERENT_CONSTRAINT }
  | "Sum" ~> "(" ~> list ~ ("," ~> arithm_exp) <~ ")" ^^ { case l ~ exp => SUM_CONSTRAINT(l, exp) }
  | "Count" ~> "(" ~> list ~ ("," ~> arithm_exp) ~ ("," ~> arithm_exp) <~ ")" ^^ { case l ~ value ~ count => COUNT_CONSTRAINT(l, value, count) }
  )

  def arithm_exp: Parser[ARITHM_EXP] = term ~ (arithm_operator1 ~ term).* ^^ { case head ~ tail => (head /: tail) { case (lhs, op ~ rhs) => ARITHM_OP_EXP(op, lhs, rhs) } }

  def term: Parser[ARITHM_EXP] = factor ~ (arithm_operator2 ~ factor).* ^^ { case head ~ tail => (head /: tail) { case (lhs, op ~ rhs) => ARITHM_OP_EXP(op, lhs, rhs) } }

  def factor: Parser[ARITHM_EXP] = (
    numeral ^^ { CONST_EXP }
  | "Abs" ~> "(" ~> arithm_exp <~ ")" ^^ { ABS_EXP }
  | var_id ^^ { VAR_EXP }
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

  def relational_equality_operator: Parser[String] = (
    "="
  | "<>"
  )

  def relational_inequality_operator: Parser[String] = (
    "=<"
  | "<"
  | ">="
  | ">"
  )

  def arithm_operator1: Parser[String] = (
    "+"
  | "-"
  )

  def arithm_operator2: Parser[String] = (
    "*"
  | "Div"
  | "Mod"
  )

  def id: Parser[IDENTIFIER] = """[A-Za-z_]\w*""".r ^^ { IDENTIFIER }

  def numeral: Parser[NUMERAL] = """\d+""".r ^^ { string => NUMERAL(string.toInt) }
}

object Parser extends Parser

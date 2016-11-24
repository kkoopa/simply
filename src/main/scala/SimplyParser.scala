/**
  * Created by Benjamin Byholm on 18.11.2016.
  */
import java.io.FileReader

import scala.util.parsing.combinator.RegexParsers

import Printer.AnyTreeString

import scala.collection.mutable

import scala.util.matching.Regex

class SimplyParser extends RegexParsers {
  override val whiteSpace : Regex = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

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
  | "Abs" ~> arithm_exp ^^ { ABS_EXP }
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

object Env {
  val env : mutable.Map[String, Int] = mutable.Map[String, Int]()
  val dom : mutable.Map[String, List[Int]] = mutable.Map[String, List[Int]]()
  val xev : mutable.Map[String, (List[Int], List[Int])] = mutable.Map[String, (List[Int], List[Int])]()
  val local : mutable.Map[String, Int] = mutable.Map[String, Int]()
}

object SimplyParserTest extends SimplyParser {
  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for (xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }

  def printAST(path: String): Unit = println(parseAll(simply_problem, new FileReader(path)).get.treeString)

  def main(args: Array[String]): Unit = {
    printAST("target/scala-2.12/classes/SchursLemma_10_3.y")
    printAST("target/scala-2.12/classes/queens_8.y")
    printAST("target/scala-2.12/classes/bacp_12_6.y")
    printAST("target/scala-2.12/classes/jobshop_58.y")
    val tree = parseAll(simply_problem, new FileReader("target/scala-2.12/classes/SchursLemma_10_3.y")).get
    def visit(node:AST) : Any = node match {
      case PROBLEM(IDENTIFIER(name), data, domains, variables, constraints) => data.map(visit); domains.map(visit); variables.map(visit); constraints.flatMap(visitSentence)
      case DATA_EXP(IDENTIFIER(name), exp:ARITHM_EXP) => val res = exp.evaluate; Env.env(name) = res
      case DATA_EXP(IDENTIFIER(name), exp:FORMULA) => val res = exp.evaluate; Env.env(name) = if (res) 1 else 0
      case DOMAIN_EXP(IDENTIFIER(name), list) => Env.dom(name) = visit(list).asInstanceOf[List[Int]]
      case LIST_COMPREHENSION(exp, restrictions) =>
        val res = restrictions.map(visit)
        val (filters:List[FORMULA], generators:List[(String, List[Int])]) = res partition { case _:FORMULA => true; case _ => false }
        val (names:List[String], domains:List[List[Int]]) = generators.unzip
        cartesianProduct(domains) map (names zip _) flatMap (x => { x.foreach(y => Env.local(y._1) = y._2); if (filters.forall(_.evaluate)) List(exp.evaluate) else Nil })
      case LIST_ENUMERATION(list) => list.flatMap(visitList)
      case MEMBER_RESTRICT(IDENTIFIER(name), list) => (name, visit(list))
      case PREDICATE_RESTRICT(predicate) => predicate
      //case LIST_ELEMENT_EXP(ex) => List(ex.evaluate)
      //case LIST_ELEMENT_RANGE(RANGE(lb, ub)) => Range.inclusive(lb.evaluate, ub.evaluate).toList
      case VARIABLE_EXP(idents, IDENTIFIER(domain)) => idents.foreach {
        case VAR_ID(IDENTIFIER(name), Nil) => Env.xev(name) = (Nil, if (domain == "__BOOL__") List(0, 1) else Env.dom(domain))
        case VAR_ID(IDENTIFIER(name), explist) => Env.xev(name) = (explist.map(_.evaluate), if (domain == "__BOOL__") List(0, 1) else Env.dom(domain))
      }
      case VAR_ID(IDENTIFIER(name), Nil) => Env.env(name)
      //case VAR_ID(IDENTIFIER(name), list) =>
    }

    def visitProblem(node:PROBLEM) : List[CONSTRAINT] = node match {
      case PROBLEM(ident, data, domains, variables, constraints) => data.map(visit); domains.map(visit); variables.map(visit); constraints.flatMap(visitSentence)
    }

    def visitSentence(sentence:SENTENCE) : List[CONSTRAINT] = sentence match {
      case SENTENCE_STATEMENT(statement) => visitStatement(statement)
      case SENTENCE_CONSTRAINT(constraint) => List(visitConstraint(constraint))
    }

    def visitStatement(statement:STATEMENT) : List[CONSTRAINT] = statement match {
      case FORALL_STATEMENT(FORALL(IDENTIFIER(name), list, sentences)) => visit(list).asInstanceOf[List[Int]].flatMap(x => { Env.local(name) = x; sentences.flatMap(visitSentence) })
      case IF_THEN_ELSE_STATEMENT(IF_THEN_ELSE(predicate, ifs, elses)) => if (predicate.evaluate) ifs.flatMap(visitSentence) else elses.flatMap(visitSentence)
    }

    def visitConstraint(constraint:CONSTRAINT) : CONSTRAINT = constraint match {
      case PREDICATE_CONSTRAINT(predicate) => PREDICATE_CONSTRAINT(predicate.simplify)
      //case IF_THEN_ELSE_CONSTRAINT(predicate, ifs, elses) =>
      //case ALLDIFFERENT_CONSTRAINT(list) => visit(list)
      //case SUM_CONSTRAINT(list, value) => visit(list)
      //case COUNT_CONSTRAINT(list, value, count) => visit(list)
    }

    def visitList(node:LIST_ELEMENT) : List[Int] = node match {
      case LIST_ELEMENT_EXP(exp) => List(exp.evaluate)
      case LIST_ELEMENT_RANGE(RANGE(lb, ub)) => Range.inclusive(lb.evaluate, ub.evaluate).toList
    }

    def pretty(node:AST) : String = node match {
      case PREDICATE_CONSTRAINT(predicate) => pretty(predicate)
      //case IF_THEN_ELSE_CONSTRAINT(predicate, ifs, elses) =>
      //case ALLDIFFERENT_CONSTRAINT(list) =>
      //case SUM_CONSTRAINT(list, value) =>
      //case COUNT_CONSTRAINT(list, value, count) =>
      case CONST_FORMULA(b) => b.toString
      case NOT_FORMULA(f) => f match {
        case BOOL_OP_FORMULA(op, lhs, rhs) => "!(" ++ pretty(BOOL_OP_FORMULA(op, lhs, rhs)) ++ ")"
        case REL_OP_FORMULA(op, lhs, rhs) => "!(" ++ pretty(REL_OP_FORMULA(op, lhs, rhs)) ++ ")"
        case _ => "!" ++ pretty(f)
      }
      case BOOL_OP_FORMULA(op, lhs, rhs) => pretty(lhs) ++ " " ++ op ++ " " ++ pretty(rhs)
      case REL_OP_FORMULA(op, lhs, rhs) => pretty(lhs) ++ " " ++ op ++ " " ++ pretty(rhs)
      case VAR_FORMULA(VAR_ID(IDENTIFIER(name), offsets)) => name ++ offsets.map("[" ++ _.evaluate.toString ++ "]").reduce(_++_)
      case CONST_EXP(NUMERAL(n)) => n.toString
      case ABS_EXP(exp) => "Abs (" ++ pretty(exp) ++ ")"
      case ARITHM_OP_EXP(op, lhs, rhs) => pretty(lhs) ++ " " ++ op ++ " " ++ pretty(rhs)
      case VAR_EXP(VAR_ID(IDENTIFIER(name), offsets)) => name ++ offsets.map("[" ++ _.evaluate.toString ++ "]").reduce(_++_)
    }

    val consts : List[CONSTRAINT] = visitProblem(tree)
    val combo = consts.map{ case PREDICATE_CONSTRAINT(p) => p }.reduce(BOOL_OP_FORMULA("And", _, _))
    println(Env.xev)
    println(consts.map(pretty) mkString "\n")
    println(pretty(PREDICATE_CONSTRAINT(combo.simplify)))
  }
}

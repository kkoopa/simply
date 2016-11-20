import scala.util.parsing.input.Positional

/**
  * Created by Benjamin Byholm on 18.11.2016.
  */
sealed trait AST extends Positional

case class IDENTIFIER(name: String) extends AST
case class NUMERAL(value: Int) extends AST

sealed trait EXPRESSION extends AST
sealed trait FORMULA extends EXPRESSION {
  def evaluate: Boolean
}
sealed trait ARITHM_EXP extends EXPRESSION {
  def evaluate: Int
}

case class PROBLEM(ident: IDENTIFIER, data: List[DATA_EXP], domains: List[DOMAIN_EXP], variables: List[VARIABLE_EXP], constraints: List[SENTENCE]) extends AST

case class DATA_EXP(ident: IDENTIFIER, exp: EXPRESSION) extends AST
case class DOMAIN_EXP(ident: IDENTIFIER, list: LIST) extends AST
case class VARIABLE_EXP(idents: List[VAR_ID], domain_ident: IDENTIFIER) extends AST

sealed trait SENTENCE extends AST
case class SENTENCE_STATEMENT(exp: STATEMENT) extends SENTENCE
case class SENTENCE_CONSTRAINT(exp: CONSTRAINT) extends SENTENCE

sealed trait STATEMENT extends AST
case class IF_THEN_ELSE_STATEMENT(statement: IF_THEN_ELSE) extends STATEMENT
case class FORALL_STATEMENT(statement: FORALL) extends STATEMENT

sealed trait CONSTRAINT extends AST
sealed trait GLOBAL_CONSTRAINT extends CONSTRAINT
case class IF_THEN_ELSE_CONSTRAINT(predicate: FORMULA, ifs: List[SENTENCE], elses: List[SENTENCE]) extends CONSTRAINT
case class PREDICATE_CONSTRAINT(predicate: FORMULA) extends CONSTRAINT

case class ALLDIFFERENT_CONSTRAINT(list: LIST) extends GLOBAL_CONSTRAINT
case class SUM_CONSTRAINT(list: LIST, value: ARITHM_EXP) extends GLOBAL_CONSTRAINT
case class COUNT_CONSTRAINT(list: LIST, value: ARITHM_EXP, count: ARITHM_EXP) extends GLOBAL_CONSTRAINT

case class IF_THEN_ELSE(predicate: FORMULA, ifs: List[SENTENCE], elses: List[SENTENCE] = Nil) extends AST
case class FORALL(ident: IDENTIFIER, list: LIST, sentences: List[SENTENCE]) extends AST

case class VAR_ID(ident: IDENTIFIER, offsets: List[ARITHM_EXP] = Nil) extends AST

sealed trait LIST extends AST
case class LIST_ENUMERATION(elements: List[LIST_ELEMENT]) extends LIST
case class LIST_COMPREHENSION(exp: ARITHM_EXP, restrictions: List[VAR_RESTRICT]) extends LIST

sealed trait LIST_ELEMENT extends AST
case class LIST_ELEMENT_RANGE(range: RANGE) extends LIST_ELEMENT
case class LIST_ELEMENT_EXP(exp: ARITHM_EXP) extends LIST_ELEMENT

sealed trait VAR_RESTRICT extends AST
case class MEMBER_RESTRICT(ident: IDENTIFIER, list: LIST) extends VAR_RESTRICT
case class PREDICATE_RESTRICT(predicate: FORMULA) extends VAR_RESTRICT

case class RANGE(lb: ARITHM_EXP, ub: ARITHM_EXP) extends AST

case class CONST_FORMULA(value: Boolean) extends FORMULA {
  override def evaluate = value
}
case class NOT_FORMULA(formula : FORMULA) extends FORMULA {
  override def evaluate = !formula.evaluate
}
case class BOOL_OP_FORMULA(op: String, lhs: FORMULA, rhs: FORMULA) extends FORMULA {
  override def evaluate = op match {
    case "And" => lhs.evaluate && rhs.evaluate
    case "Or" => lhs.evaluate || rhs.evaluate
    case "Xor" => lhs.evaluate ^ rhs.evaluate
    case "Iff" => !(lhs.evaluate ^ rhs.evaluate)
    case "Implies" => !lhs.evaluate || rhs.evaluate
  }
}
case class REL_OP_FORMULA(op: String, lhs: ARITHM_EXP, rhs: ARITHM_EXP) extends FORMULA {
  override def evaluate = op match {
    case "=" => lhs.evaluate == rhs.evaluate
    case "<>" => lhs.evaluate != rhs.evaluate
    case "<" => lhs.evaluate < rhs.evaluate
    case ">" => lhs.evaluate > rhs.evaluate
    case "=<" => lhs.evaluate <= rhs.evaluate
    case ">=" => lhs.evaluate >= rhs.evaluate
  }
}

object Env {
  val env = collection.mutable.Map[String, collection.mutable.Map[Int, Int]]()
}
case class VAR_FORMULA(var_id: VAR_ID) extends FORMULA {
  override def evaluate = var_id match {
    case VAR_ID(IDENTIFIER(s), Nil) => Env.env(s)(0) != 0
    case VAR_ID(IDENTIFIER(s), offsets) => offsets.map(x => x.evaluate).map(Env.env(s)(_) != 0).reduce(_ && _)
  }
}

case class ARITHM_OP_EXP(op: String, lhs: ARITHM_EXP, rhs: ARITHM_EXP) extends ARITHM_EXP {
  override def evaluate = op match {
    case "+" => lhs.evaluate + rhs.evaluate
    case "-" => lhs.evaluate - rhs.evaluate
    case "*" => lhs.evaluate * rhs.evaluate
    case "Div" => lhs.evaluate / rhs.evaluate
    case "Mod" => lhs.evaluate % rhs.evaluate
  }
}
case class ABS_EXP(exp: ARITHM_EXP) extends ARITHM_EXP {
  override def evaluate = math.abs(exp.evaluate)
}
case class CONST_EXP(value: NUMERAL) extends ARITHM_EXP {
  override def evaluate = value match { case NUMERAL(n) => n }
}
case class VAR_EXP(var_id: VAR_ID) extends ARITHM_EXP {
  override def evaluate = var_id match {
    case VAR_ID(IDENTIFIER(s), Nil) => Env.env(s)(0)
    case VAR_ID(IDENTIFIER(s), offsets) => offsets.map(x => x.evaluate).map(Env.env(s)(_)).sum
  }
}

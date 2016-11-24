import scala.util.parsing.input.Positional

/**
  * Created by Benjamin Byholm on 18.11.2016.
  */
sealed trait AST extends Positional

case class IDENTIFIER(name: String) extends AST
case class NUMERAL(value: Int) extends AST

sealed trait EXPRESSION extends AST {
  def simplify : EXPRESSION
}
sealed trait FORMULA extends EXPRESSION {
  def evaluate: Boolean
  override def simplify: FORMULA
}
sealed trait ARITHM_EXP extends EXPRESSION {
  def evaluate: Int
  override def simplify: ARITHM_EXP
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
  override def evaluate : Boolean = value
  override def simplify : FORMULA = this
}
case class NOT_FORMULA(formula : FORMULA) extends FORMULA {
  override def evaluate : Boolean = !formula.evaluate
  override def simplify : FORMULA = formula.simplify match {
    case CONST_FORMULA(b) => CONST_FORMULA(!b)
    case NOT_FORMULA(f) => f
    case r => NOT_FORMULA(r)
  }
}
case class BOOL_OP_FORMULA(op: String, lhs: FORMULA, rhs: FORMULA) extends FORMULA {
  override def evaluate : Boolean = op match {
    case "And" => lhs.evaluate && rhs.evaluate
    case "Or" => lhs.evaluate || rhs.evaluate
    case "Xor" => lhs.evaluate ^ rhs.evaluate
    case "Iff" => !(lhs.evaluate ^ rhs.evaluate)
    case "Implies" => !lhs.evaluate || rhs.evaluate
  }
  override def simplify : FORMULA = {
    val newlhs = lhs.simplify
    val newrhs = rhs.simplify
    op match {
      case "And" =>
        if (newlhs == newrhs) newlhs
        else (newlhs, newrhs) match {
          case (CONST_FORMULA(false), _) => CONST_FORMULA(false)
          case (_, CONST_FORMULA(false)) => CONST_FORMULA(false)
          case (CONST_FORMULA(true), r) => r
          case (r, CONST_FORMULA(true)) => r
          case (p1, BOOL_OP_FORMULA("Or", p2, q)) => if (p1 == p2) p1 else BOOL_OP_FORMULA("And", newlhs, newrhs)
          case (p1, BOOL_OP_FORMULA("Or", q, p2)) => if (p1 == p2) p1 else BOOL_OP_FORMULA("And", newlhs, newrhs)
          case (BOOL_OP_FORMULA("Or", p2, q), p1) => if (p1 == p2) p1 else BOOL_OP_FORMULA("And", newlhs, newrhs)
          case (BOOL_OP_FORMULA("Or", q, p2), p1) => if (p1 == p2) p1 else BOOL_OP_FORMULA("And", newlhs, newrhs)
          case (a, b) => BOOL_OP_FORMULA("And", a, b)
        }
      case "Or" =>
        if (newlhs == newrhs) newlhs
        else (newlhs, newrhs) match {
          case (CONST_FORMULA(false), r) => r
          case (r, CONST_FORMULA(false)) => r
          case (p1, BOOL_OP_FORMULA("And", p2, q)) => if (p1 == p2) p1 else BOOL_OP_FORMULA("Or", newlhs, newrhs)
          case (p1, BOOL_OP_FORMULA("And", q, p2)) => if (p1 == p2) p1 else BOOL_OP_FORMULA("Or", newlhs, newrhs)
          case (BOOL_OP_FORMULA("And", p2, q), p1) => if (p1 == p2) p1 else BOOL_OP_FORMULA("Or", newlhs, newrhs)
          case (BOOL_OP_FORMULA("And", q, p2), p1) => if (p1 == p2) p1 else BOOL_OP_FORMULA("Or", newlhs, newrhs)
          case (CONST_FORMULA(true), _) => CONST_FORMULA(true)
          case (_, CONST_FORMULA(true)) => CONST_FORMULA(true)
          case (a, b) => BOOL_OP_FORMULA("Or", a, b)
        }
      case "Xor" => BOOL_OP_FORMULA("Or", BOOL_OP_FORMULA("And", lhs, NOT_FORMULA(rhs)), BOOL_OP_FORMULA("And", NOT_FORMULA(lhs), rhs)).simplify
      case "Iff" => NOT_FORMULA(BOOL_OP_FORMULA("Xor", lhs, rhs)).simplify
      case "Implies" => BOOL_OP_FORMULA("Or", NOT_FORMULA(lhs), rhs).simplify
    }
  }
}
case class REL_OP_FORMULA(op: String, lhs: ARITHM_EXP, rhs: ARITHM_EXP) extends FORMULA {
  override def evaluate : Boolean = op match {
    case "=" => lhs.evaluate == rhs.evaluate
    case "<>" => lhs.evaluate != rhs.evaluate
    case "<" => lhs.evaluate < rhs.evaluate
    case ">" => lhs.evaluate > rhs.evaluate
    case "=<" => lhs.evaluate <= rhs.evaluate
    case ">=" => lhs.evaluate >= rhs.evaluate
  }
  override def simplify : FORMULA = {
    val newlhs = lhs.simplify
    val newrhs = rhs.simplify
    op match {
      case "=" => if (newlhs == newrhs) CONST_FORMULA(true)
      else (newlhs, newrhs) match {
        case (CONST_EXP(_), CONST_EXP(_)) => CONST_FORMULA(false)
        case _ => REL_OP_FORMULA("=", newlhs, newrhs)
      }
      case "<>" => if (newlhs == newrhs) CONST_FORMULA(false)
      else (newlhs, newrhs) match {
        case (CONST_EXP(_), CONST_EXP(_)) => CONST_FORMULA(true)
        case _ => REL_OP_FORMULA("<>", newlhs, newrhs)
      }
      case "<" => (newlhs, newrhs) match {
        case (CONST_EXP(NUMERAL(n)), CONST_EXP(NUMERAL(m))) => CONST_FORMULA(n < m)
        case _ => REL_OP_FORMULA("<", newlhs, newrhs)
      }
      case ">" => (newlhs, newrhs) match {
        case (CONST_EXP(NUMERAL(n)), CONST_EXP(NUMERAL(m))) => CONST_FORMULA(n > m)
        case _ => REL_OP_FORMULA(">", newlhs, newrhs)
      }
      case "=<" => (newlhs, newrhs) match {
        case (CONST_EXP(NUMERAL(n)), CONST_EXP(NUMERAL(m))) => CONST_FORMULA(n <= m)
        case _ => REL_OP_FORMULA("=<", newlhs, newrhs)
      }
      case ">=" => (newlhs, newrhs) match {
        case (CONST_EXP(NUMERAL(n)), CONST_EXP(NUMERAL(m))) => CONST_FORMULA(n >= m)
        case _ => REL_OP_FORMULA(">=", newlhs, newrhs)
      }
    }
  }
}

case class VAR_FORMULA(var_id: VAR_ID) extends FORMULA {
  override def evaluate : Boolean = var_id match {
    case VAR_ID(IDENTIFIER(s), Nil) => Env.env(s) != 0
  }
  override def simplify : FORMULA = var_id match {
    case VAR_ID(IDENTIFIER(name), Nil) =>
      var foo = Env.local.get(name)
      if (foo.isEmpty) foo = Env.env.get(name)
      if (foo.isEmpty) return this
      CONST_FORMULA(foo.get != 0)
    case VAR_ID(ident, list) => VAR_FORMULA(VAR_ID(ident, list.map(_.simplify)))
  }
}

case class ARITHM_OP_EXP(op: String, lhs: ARITHM_EXP, rhs: ARITHM_EXP) extends ARITHM_EXP {
  override def evaluate : Int = op match {
    case "+" => lhs.evaluate + rhs.evaluate
    case "-" => lhs.evaluate - rhs.evaluate
    case "*" => lhs.evaluate * rhs.evaluate
    case "Div" => lhs.evaluate / rhs.evaluate
    case "Mod" => lhs.evaluate % rhs.evaluate
  }
  override def simplify : ARITHM_EXP = {
    val newlhs = lhs.simplify
    val newrhs = rhs.simplify
    op match {
      case "+" => (newlhs, newrhs) match {
        case (CONST_EXP(NUMERAL(a)), CONST_EXP(NUMERAL(b))) => CONST_EXP(NUMERAL(a + b))
        case (CONST_EXP(NUMERAL(0)), r) => r
        case (r, CONST_EXP(NUMERAL(0))) => r
        case (ARITHM_OP_EXP("+", CONST_EXP(NUMERAL(n)), r), CONST_EXP(NUMERAL(m))) => ARITHM_OP_EXP("+", CONST_EXP(NUMERAL(n + m)), r)
        case (ARITHM_OP_EXP("+", r, CONST_EXP(NUMERAL(n))), CONST_EXP(NUMERAL(m))) => ARITHM_OP_EXP("+", r.simplify, CONST_EXP(NUMERAL(n + m)))
        case (CONST_EXP(NUMERAL(m)), ARITHM_OP_EXP("+", CONST_EXP(NUMERAL(n)), r)) => ARITHM_OP_EXP("+", CONST_EXP(NUMERAL(n + m)), r)
        case (CONST_EXP(NUMERAL(m)), ARITHM_OP_EXP("+", r, CONST_EXP(NUMERAL(n)))) => ARITHM_OP_EXP("+", r.simplify, CONST_EXP(NUMERAL(n + m)))
        case (ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(n)), r), CONST_EXP(NUMERAL(m))) => ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(n + m)), r)
        case (ARITHM_OP_EXP("-", r, CONST_EXP(NUMERAL(n))), CONST_EXP(NUMERAL(m))) => ARITHM_OP_EXP("-", r.simplify, CONST_EXP(NUMERAL(n - m)))
        case (CONST_EXP(NUMERAL(m)), ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(n)), r)) => ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(m + n)), r)
        case (CONST_EXP(NUMERAL(m)), ARITHM_OP_EXP("-", r, CONST_EXP(NUMERAL(n)))) => ARITHM_OP_EXP("-", r.simplify, CONST_EXP(NUMERAL(n - m)))
        case _ => ARITHM_OP_EXP("+", newlhs, newrhs)
      }
      case "-" => (newlhs, newrhs) match {
        case (CONST_EXP(NUMERAL(a)), CONST_EXP(NUMERAL(b))) => CONST_EXP(NUMERAL(a - b))
        case (r, CONST_EXP(NUMERAL(0))) => r
        case (ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(n)), r), CONST_EXP(NUMERAL(m))) => ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(n - m)), r)
        case (ARITHM_OP_EXP("-", r, CONST_EXP(NUMERAL(n))), CONST_EXP(NUMERAL(m))) => ARITHM_OP_EXP("-", r, CONST_EXP(NUMERAL(n + m)))
        case (CONST_EXP(NUMERAL(m)), ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(n)), r)) => ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(m - n)), r)
        case (CONST_EXP(NUMERAL(m)), ARITHM_OP_EXP("-", r, CONST_EXP(NUMERAL(n)))) => ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(m + n)), r)
        case (ARITHM_OP_EXP("+", CONST_EXP(NUMERAL(n)), r), CONST_EXP(NUMERAL(m))) => ARITHM_OP_EXP("+", CONST_EXP(NUMERAL(n - m)), r)
        case (ARITHM_OP_EXP("+", r, CONST_EXP(NUMERAL(n))), CONST_EXP(NUMERAL(m))) => ARITHM_OP_EXP("+", r, CONST_EXP(NUMERAL(n - m)))
        case (CONST_EXP(NUMERAL(m)), ARITHM_OP_EXP("+", CONST_EXP(NUMERAL(n)), r)) => ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(m - n)), r)
        case (CONST_EXP(NUMERAL(m)), ARITHM_OP_EXP("+", r, CONST_EXP(NUMERAL(n)))) => ARITHM_OP_EXP("-", CONST_EXP(NUMERAL(m - n)), r)
        case _ => ARITHM_OP_EXP("-", newlhs, newrhs)
      }
      case "*" => (newlhs, newrhs) match {
        case (CONST_EXP(NUMERAL(a)), CONST_EXP(NUMERAL(b))) => CONST_EXP(NUMERAL(a * b))
        case (CONST_EXP(NUMERAL(0)), r) => CONST_EXP(NUMERAL(0))
        case (r, CONST_EXP(NUMERAL(0))) => CONST_EXP(NUMERAL(0))
        case (CONST_EXP(NUMERAL(1)), r) => r.simplify
        case (r, CONST_EXP(NUMERAL(1))) => r.simplify
        case _ => ARITHM_OP_EXP("*", newlhs, newrhs)
      }
      case "Div" => (newlhs, newrhs) match {
        case (CONST_EXP(NUMERAL(a)), CONST_EXP(NUMERAL(b))) => CONST_EXP(NUMERAL(a / b))
        case (CONST_EXP(NUMERAL(0)), r) => CONST_EXP(NUMERAL(0))
        //case (r, CONST_EXP(NUMERAL(0))) => CONST_EXP(NUMERAL(0))
        case (CONST_EXP(NUMERAL(1)), r) => CONST_EXP(NUMERAL(1))
        case (r, CONST_EXP(NUMERAL(1))) => r.simplify
        case _ => ARITHM_OP_EXP("Div", newlhs, newrhs)
      }
      case "Mod" => (newlhs, newrhs) match {
        case (CONST_EXP(NUMERAL(a)), CONST_EXP(NUMERAL(b))) => CONST_EXP(NUMERAL(a % b))
        case (CONST_EXP(NUMERAL(0)), r) => CONST_EXP(NUMERAL(0))
        //case (r, CONST_EXP(NUMERAL(0))) => CONST_EXP(NUMERAL(0))
        case (r, CONST_EXP(NUMERAL(1))) => CONST_EXP(NUMERAL(0))
        case _ => ARITHM_OP_EXP("Mod", newlhs, newrhs)
      }
    }
  }
}
case class ABS_EXP(exp: ARITHM_EXP) extends ARITHM_EXP {
  override def evaluate : Int = math.abs(exp.evaluate)
  override def simplify : ARITHM_EXP = {
    val simpexp = exp.simplify
    simpexp match {
      case CONST_EXP(NUMERAL(n)) => CONST_EXP(NUMERAL(n))
      case _ => ABS_EXP(simpexp)
    }
  }
}
case class CONST_EXP(value: NUMERAL) extends ARITHM_EXP {
  override def evaluate : Int = value match { case NUMERAL(n) => n }
  override def simplify : ARITHM_EXP = this
}
case class VAR_EXP(var_id: VAR_ID) extends ARITHM_EXP {
  override def evaluate : Int = var_id match {
    case VAR_ID(IDENTIFIER(s), Nil) => Env.local.getOrElse(s, Env.env(s))
  }
  override def simplify : ARITHM_EXP = var_id match {
    case VAR_ID(IDENTIFIER(name), Nil) =>
      var foo = Env.local.get(name)
      if (foo.isEmpty) foo = Env.env.get(name)
      if (foo.isEmpty) return this
      CONST_EXP(NUMERAL(foo.get))
    case VAR_ID(ident, list) => VAR_EXP(VAR_ID(ident, list.map(_.simplify))
    )
  }
}

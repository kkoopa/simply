/**
  * Created by Benjamin Byholm on 19.11.2016.
  */
object Printer {
  implicit class AnyTreeString[A](a: A) {

    private def indent(s: String)
    = s.lines.toStream match {
      case h +: t =>
        (("- " + h) +: t.map {
          "| " + _
        }) mkString "\n"
      case _ => "- "
    }

    /**
      * @return A readable string representation of this value
      */
    def treeString
    : String
    = a match {
      case x: Traversable[_] =>
        x.stringPrefix + ":\n" +
          x.view
            .map {
              _.treeString
            }
            .map {
              indent
            }
            .mkString("\n")
      case x: Product if x.productArity == 0 =>
        x.productPrefix
      case x: Product =>
        x.productPrefix + ":\n" +
          x.productIterator
            .map {
              _.treeString
            }
            .map {
              indent
            }
            .mkString("\n")
      case null =>
        "null"
      case _ =>
        a.toString
    }
  }

  def pretty(node:AST) : String = node match {
    case PREDICATE_CONSTRAINT(predicate) => pretty(predicate)
    case IF_THEN_ELSE_CONSTRAINT(predicate, ifs, elses) => "If (" ++ pretty(predicate) ++ ") { " ++ (ifs map pretty).mkString(", ") ++ " } else { " ++ (elses map pretty).mkString(", ") ++ " }"
    case ALLDIFFERENT_CONSTRAINT(list) => "Alldifferent (" ++ pretty(list) ++ ")"
    case SUM_CONSTRAINT(list, value) => "Sum (" ++ pretty(list) ++ ", " ++ pretty(value) ++ ")"
    case COUNT_CONSTRAINT(list, value, count) => "Count (" ++ pretty(list) ++ ", " ++ pretty(value) ++ ", " ++ pretty(count) ++ ")"
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
    case LIST_ELEMENT_EXP(ex) => pretty(ex)
    case LIST_ENUMERATION(l) => "[" ++ l.map(pretty).mkString(", ") ++ "]"
    case _ => throw new Error("Bad code!")
  }
}

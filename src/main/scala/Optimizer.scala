/**
  * Created by Benjamin Byholm on 25.11.2016.
  */
class Optimizer {
  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for (xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }

  def visitProblem(node:PROBLEM) : List[CONSTRAINT] = node match {
    case PROBLEM(ident, data, domains, variables, constraints) => data.foreach(visitData); domains.foreach(visitDomain); variables.foreach(visitVariable); constraints.flatMap(visitSentence)
  }

  def visitData(data:DATA_EXP) : Unit = data match {
    case DATA_EXP(IDENTIFIER(name), exp:ARITHM_EXP) => val res = exp.evaluate; Env.env(name) = res
    case DATA_EXP(IDENTIFIER(name), exp:FORMULA) => val res = exp.evaluate; Env.env(name) = if (res) 1 else 0
  }

  def visitDomain(domain:DOMAIN_EXP) : Unit = domain match {
    case DOMAIN_EXP(IDENTIFIER(name), list) => Env.dom(name) = visitList(list)
  }

  def visitVariable(variable:VARIABLE_EXP) : Unit = variable match {
    case VARIABLE_EXP(idents, IDENTIFIER(domain)) => idents.foreach {
      case VAR_ID(IDENTIFIER(name), Nil) => Env.xev(name) = (Nil, if (domain == "__BOOL__") List(0, 1) else Env.dom(domain))
      case VAR_ID(IDENTIFIER(name), explist) => Env.xev(name) = (explist.map(_.evaluate), if (domain == "__BOOL__") List(0, 1) else Env.dom(domain))
    }
  }

  def visitVarId(varid:VAR_ID) : Unit = varid match {
    case VAR_ID(IDENTIFIER(name), Nil) => Env.env(name)
    case VAR_ID(IDENTIFIER(name), list) => throw new Error("Bad code!")
  }

  def visitList(list:LIST) : List[Int] = list match {
    case LIST_COMPREHENSION(exp, restrictions) =>
      val (a, b) = restrictions partition { case MEMBER_RESTRICT(_, _) => true; case _ => false }
      val filters = b.asInstanceOf[List[PREDICATE_RESTRICT]] map { case PREDICATE_RESTRICT(predicate) => predicate }
      val (names, domains) = (a.asInstanceOf[List[MEMBER_RESTRICT]] map { case MEMBER_RESTRICT(IDENTIFIER(name), alist) => (name, visitList(alist)) }).unzip
      cartesianProduct(domains) map (names zip _) flatMap (x => { x.foreach(y => Env.local(y._1) = y._2); if (filters.forall(_.evaluate)) List(exp.evaluate) else Nil })
    case LIST_ENUMERATION(alist) => alist.flatMap(visitListElement)
  }

  def visitListElement(node:LIST_ELEMENT) : List[Int] = node match {
    case LIST_ELEMENT_EXP(exp) => List(exp.evaluate)
    case LIST_ELEMENT_RANGE(RANGE(lb, ub)) => Range.inclusive(lb.evaluate, ub.evaluate).toList
  }

  def visitListLazy(list:LIST) : List[ARITHM_EXP] = list match {
    case LIST_COMPREHENSION(exp, restrictions) =>
      val (a, b) = restrictions partition { case MEMBER_RESTRICT(_, _) => true; case PREDICATE_RESTRICT(_) => false }
      val filters = b.asInstanceOf[List[PREDICATE_RESTRICT]] map { case PREDICATE_RESTRICT(predicate) => predicate }
      val (names, domains) = (a.asInstanceOf[List[MEMBER_RESTRICT]] map { case MEMBER_RESTRICT(IDENTIFIER(name), alist) => (name, visitList(alist)) }).unzip
      cartesianProduct(domains) map (names zip _) flatMap (x => { x.foreach(y => Env.local(y._1) = y._2); if (filters.forall(_.evaluate)) List(exp.simplify) else Nil })
    case LIST_ENUMERATION(alist) => alist.flatMap(visitListElementLazy)
  }

  def visitListElementLazy(node:LIST_ELEMENT) : List[ARITHM_EXP] = node match {
    case LIST_ELEMENT_EXP(exp) => List(exp.simplify)
    case LIST_ELEMENT_RANGE(RANGE(lb, ub)) => Range.inclusive(lb.evaluate, ub.evaluate).map(x => CONST_EXP(NUMERAL(x))).toList
  }

  def visitSentence(sentence:SENTENCE) : List[CONSTRAINT] = sentence match {
    case SENTENCE_STATEMENT(statement) => visitStatement(statement)
    case SENTENCE_CONSTRAINT(constraint) => List(visitConstraint(constraint))
  }

  def visitStatement(statement:STATEMENT) : List[CONSTRAINT] = statement match {
    case FORALL_STATEMENT(FORALL(IDENTIFIER(name), list, sentences)) => visitList(list).flatMap(x => { Env.local(name) = x; sentences.flatMap(visitSentence) })
    case IF_THEN_ELSE_STATEMENT(IF_THEN_ELSE(predicate, ifs, elses)) => if (predicate.evaluate) ifs.flatMap(visitSentence) else elses.flatMap(visitSentence)
  }

  def visitConstraint(constraint:CONSTRAINT) : CONSTRAINT = constraint match {
    case PREDICATE_CONSTRAINT(predicate) => PREDICATE_CONSTRAINT(predicate.simplify)
    case IF_THEN_ELSE_CONSTRAINT(predicate, ifs, elses) =>
      val if_constraints = ifs.flatMap(visitSentence).asInstanceOf[List[PREDICATE_CONSTRAINT]]
      val if_predicates = if_constraints.map{ case PREDICATE_CONSTRAINT(p) => p }.reduce(BOOL_OP_FORMULA("And", _, _))
      val else_constraints = elses.flatMap(visitSentence).asInstanceOf[List[PREDICATE_CONSTRAINT]]
      val else_predicates = else_constraints.map{ case PREDICATE_CONSTRAINT(p) => p }.reduce(BOOL_OP_FORMULA("And", _, _))
      PREDICATE_CONSTRAINT(BOOL_OP_FORMULA("Or", BOOL_OP_FORMULA("And", predicate, if_predicates), BOOL_OP_FORMULA("And", NOT_FORMULA(predicate), else_predicates)).simplify)
    case ALLDIFFERENT_CONSTRAINT(list) =>
      val foo:List[FORMULA] = visitListLazy(list).combinations(2).map { case Seq(x, y) => REL_OP_FORMULA("<>", x, y) }.toList
      PREDICATE_CONSTRAINT(foo.reduce(BOOL_OP_FORMULA("And", _, _)))
    case SUM_CONSTRAINT(list, value) => PREDICATE_CONSTRAINT(REL_OP_FORMULA("=", visitListLazy(list).reduce(ARITHM_OP_EXP("+", _, _)), value).simplify)
    case COUNT_CONSTRAINT(list, value, count) =>
      COUNT_CONSTRAINT(LIST_ENUMERATION(visitListLazy(list).map(LIST_ELEMENT_EXP)), value.simplify, count.simplify)
    /*
    val relations = visitListLazy(list).map(REL_OP_FORMULA("=", _, value))
    def gadget(x:List[REL_OP_FORMULA], c:Int) : FORMULA = (x, c) match {
      case (Nil, 0) => CONST_FORMULA(true)
      case (Nil, _) => CONST_FORMULA(false)
      case (h :: t, 0) => BOOL_OP_FORMULA("And", gadget(t, 0), NOT_FORMULA(h))
      case (h :: t, n) => BOOL_OP_FORMULA("Or", BOOL_OP_FORMULA("And", gadget(t, n - 1), h), BOOL_OP_FORMULA("And", gadget(t, n), NOT_FORMULA(h)))
    }
    PREDICATE_CONSTRAINT(gadget(relations, count.evaluate).simplify)
    */
  }
}

object Optimizer extends Optimizer

/**
  * Created by Benjamin Byholm on 25.11.2016.
  */

import java.io.FileReader

import Printer.AnyTreeString

object Compiler {

  def printAST(path: String): Unit = println(Parser.parseAll(Parser.simply_problem, new FileReader(path)).get.treeString)

  def compile(path : String): String = {
    Env.clear()
    val tree = Parser.parseAll(Parser.simply_problem, new FileReader(path)).get
    val consts = Optimizer.visitProblem(tree)
    var res = ""
    for ((name, (dimensions, domain)) <- Env.xev) {
      res = res ++ name ++ dimensions.flatMap("[" ++ _.toString ++ "]") ++ "::[" ++ domain.mkString(", ") ++ "]\n"
    }
    res ++ (consts.map(Printer.pretty) mkString "\n") ++ "\n"
  }

  def main(args: Array[String]): Unit = {
    /*
    printAST("target/scala-2.12/classes/SchursLemma_10_3.y")
    printAST("target/scala-2.12/classes/queens_8.y")
    printAST("target/scala-2.12/classes/bacp_12_6.y")
    printAST("target/scala-2.12/classes/jobshop_58.y")
    */
    print(compile("target/scala-2.12/classes/SchursLemma_10_3.y"))
    println("--------------------------------------------------------------------------------")
    print(compile("target/scala-2.12/classes/queens_8.y"))
    println("--------------------------------------------------------------------------------")
    print(compile("target/scala-2.12/classes/bacp_12_6.y"))
    println("--------------------------------------------------------------------------------")
    print(compile("target/scala-2.12/classes/jobshop_58.y"))
  }
}

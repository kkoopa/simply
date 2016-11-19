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
}

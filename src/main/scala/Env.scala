/**
  * Created by Benjamin Byholm on 25.11.2016.
  */

import scala.collection.mutable

object Env {
  val env : mutable.Map[String, Int] = mutable.Map[String, Int]()
  val dom : mutable.Map[String, List[Int]] = mutable.Map[String, List[Int]]()
  val xev : mutable.Map[String, (List[Int], List[Int])] = mutable.Map[String, (List[Int], List[Int])]()
  val local : mutable.Map[String, Int] = mutable.Map[String, Int]()

  def clear() : Unit = { env.clear; dom.clear; xev.clear; local.clear }
}

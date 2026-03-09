package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.simple.*
import propel.evaluator.egraph.mutable.simple.Type.*
import collection.mutable.{Map as MutableMap, HashMap as MutableHashMap}

/**
  * [[SimpleVarsAnalysis]]
  * Goal: Identify nodes that represent variables in the SimpleLanguage.
  */
class SimpleVarsAnalysis(varList: MutableHashMap[String, Type] = MutableHashMap()) extends Analysis {
  /**
    * [[Data]] set as [[Option[Type]]] to represent the type of the variable, if it is one.
    */
  type Data = Option[Type]
  val eclass_data: MutableMap[EClass.Id, Data] = MutableMap()

  /**
    * [[GlobalData]] set as [[Boolean]] to represent inconsistencies.
    */
  type GlobalData = Boolean
  var global_data: GlobalData = false

  val dependencies = scala.List()

  /**
    * Goal: Recognize if a node is a variable and return its type.
    *
    * @param egraph graph
    * @param x node
    * @return the type of the variable, or None if not a variable
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    // A variable in SimpleLanguage is typically an ENode with no children (refs)
    if (x.refs.isEmpty) {
      return varList.get(x.op.toString())
    }
    None
  }

  /**
    * Goal: Merge variable types.
    *
    * @param data1 data
    * @param data2 data
    * @return the merged type
    */
  def merge(data1: Data, data2: Data): Data = {
    (data1, data2) match {
      case (Some(t1), Some(t2)) =>
        if (t1 != t2) {
          global_data = true
          println(s"ERROR: Variable type mismatch during merge! Cannot merge $t1 with $t2")
        }
        Some(t1)
      case _ => data1.orElse(data2)
    }
  }

  /**
    * Goal: Optional transformation.
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = ()

  /**
    * Goal: Mapping [[Op]] to variable status.
    */
  override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None): Option[Function1[Seq[Data], Data]] = {
    // In SimpleLanguage, operations (function calls, constructors with args) are not variables.
    Some(_ => None)
  }
}

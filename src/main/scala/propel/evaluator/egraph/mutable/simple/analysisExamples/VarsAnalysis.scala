package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps, Op, LType}
import propel.evaluator.egraph.mutable.simple.LType.*
import propel.evaluator.egraph.mutable.simple.Op.*
import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import propel.evaluator.egraph.EClass.Id

/**
  * [[]]
  * Goal: Identify nodes that represent variables.
  */
class VarsAnalysis(varList: MutableHashMap[String, LType] = MutableHashMap()) extends Analysis {
  /**
    * [[Data]] set as [[Boolean]].
    */
  type Data = Boolean
  val eclass_data = MutableMap()

    /**
     * [[GlobalData]] set as [[Boolean]] to represent the presence of an error/inconsistency.
     */
  type GlobalData = Boolean
  var global_data = false

  val dependencies = scala.List()

  val varTypes = varList

  /**
    * Goal: Recognize if a node is a variable.
    *
    * @param g graph
    * @param x node
    * @return if x is a variable
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    // Check if var is known (i.e. in list)
    if (varList.contains(x.op.toString())) return true
    return false
  }

  /**
    * Goal: Nothing.
    *
    * @param data1 data
    * @param data2 data
    * @return
    */
  def merge(data1: Data, data2: Data): Data = return data1 && data2
  
  /**
    * Goal: Nothing.
    *
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    return
  }

  // NOTE: This is a hack to get rid of an error ... this doesn't do anything but needs to be implemented by default
  override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None): Option[Function1[Seq[Data], Data]] = {
    op match {
      case _ => Some(args => { println(s"VarsAnalysis operation is empty, continuing ..."); true })
    }
  }
}
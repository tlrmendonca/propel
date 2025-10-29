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
class IdAnalysis(varList: MutableHashMap[String, LType] = MutableHashMap()) extends Analysis {
  /**
    * [[Data]] set as [[Eclass.Id]].
    */
  type Data = EClass.Id
  val eclass_data = MutableMap()

    /**
     * [[GlobalData]] set as [[Boolean]] though it is irrelevant here.
     */
  type GlobalData = Boolean
  var global_data = false

  val dependencies = scala.List()

  /**
    * Goal: Assign Id to node.
    *
    * @param g graph
    * @param x node
    * @return x's EClass Id.
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    val xc = egraph.find(EClass(x))
    return xc.id
  }

  /**
    * Goal: Find canonical class id.
    *
    * @param data1 data
    * @param data2 data
    * @return
    *
    * NOTE: data1 is the surviving class's id, data2 is the merged class's id.  
    */
  def merge(data1: Data, data2: Data): Data = return data1
  
  /**
    * Goal: Re-set class data to its own id (may be broken from merging?).
    *
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    val eclass = egraph.getEClassFromId(id)
    val canonical_eclass = egraph.find(eclass)
    eclass_data(id) = canonical_eclass.id
    return
  }

  // NOTE: This is a hack to get rid of an error ... this doesn't do anything but needs to be implemented by default
  override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None): Option[Function1[Seq[Data], Data]] = {
    op match {
      case _ => None
    }
  }
}
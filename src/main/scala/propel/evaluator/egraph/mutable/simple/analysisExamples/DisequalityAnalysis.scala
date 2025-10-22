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
class DisequalityAnalysis() extends Analysis {
  /**
    * [[Data]] set as [[Boolean]].
    */
  type Data = Set[EClass.Id]
  val eclass_data = MutableMap()

    /**
     * [[GlobalData]] set as [[Boolean]] to represent the presence of an error/inconsistency.
     */
  type GlobalData = Boolean
  var global_data = false

  val dependencies = scala.List()


  /**
    * Goal: Create an empty set.
    *
    * @param g graph
    * @param x node
    * @return empty set.
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    val xc = egraph.find(EClass(x))
    val emptySet = Set[EClass.Id]()
    return emptySet
  }

  /**
    * Goal: Union the sets.
    *
    * @param data1 data
    * @param data2 data
    * @return
    */
  def merge(data1: Data, data2: Data): Data = {
    return data1.union(data2)
  }
  
  /**
    * Goal: Nothing.
    *
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    return
  }

  // Extension

  /**
    * Goal: Add a disequality between two EClasses.
    * 
    * @param id the id of the first EClass.
    * @param otherId the id of the second EClass.
    */
  def disunion(id: EClass.Id, otherId: EClass.Id): Unit = {
    // add otherId to the disequality set of id
    val data = this.getData(id).get
    val newData = data + otherId
    this.setData(id, newData)

    // add id to the disequality set of otherId
    val otherData = this.getData(otherId).get
    val newOtherData = otherData + id
    this.setData(otherId, newOtherData)
  }

  /**
    * Goal: Check any insconsistencies are found in the egraph.
    */
  def is_consistent[G](egraph: G)(using EGraphOps[G]): Boolean = {
    egraph.eclasses.foreach(eclass => 
      val eclass_id = eclass.id
      val diseq_set = this.getData(eclass_id).get

      if diseq_set
        .map(id => egraph.find(EClass(id)).id) // map each id in diseq_set to its canonical id
        .contains(eclass_id) // if eclass is disequal to itself
        then
        return false // inconsistency found
    )
  }

  // NOTE: This is a hack to get rid of an error ... this doesn't do anything but needs to be implemented by default
  override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None): Option[Function1[Seq[Data], Data]] = {
    op match {
      case _ => Some(args => { println(s"DisequalityAnalysis operation is empty, continuing ..."); true })
    }
  }
}
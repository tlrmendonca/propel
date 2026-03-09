package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}
import propel.evaluator.egraph.mutable.simple.*
import propel.evaluator.egraph.mutable.simple.Type.*
import propel.evaluator.egraph.mutable.simple.ConstructorName.*
import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import propel.evaluator.egraph.EClass.Id

/**
  * [[]]
  * Goal: Identify nodes that represent variables.
  */
class DisequalityAnalysis(id_analysis: IdAnalysis) extends Analysis {
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

  val dependencies = scala.List(id_analysis)


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
    val ids = preMergeData(id_analysis).asInstanceOf[(EClass.Id, EClass.Id)]
    
    // Safety check: ensure both IDs exist in eclass_data
    val set1 = eclass_data.getOrElse(ids._1, Set.empty[EClass.Id])
    val set2 = eclass_data.getOrElse(ids._2, Set.empty[EClass.Id])

    set1.foreach(diseq_id =>
      if diseq_id == ids._2 then
        global_data = true // inconsistency found
    )
    set2.foreach(diseq_id =>
      if diseq_id == ids._1 then
        global_data = true // inconsistency found
    )
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
    return global_data
  }

  // NOTE: This is a hack to get rid of an error ... this doesn't do anything but needs to be implemented by default
  override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None): Option[Function1[Seq[Data], Data]] = {
    op match {
      case _ => None
    }
  }
}
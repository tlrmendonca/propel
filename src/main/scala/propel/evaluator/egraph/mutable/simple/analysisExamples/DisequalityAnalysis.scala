package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

/**
  * [[Disequalities between Classes]]
  * Level: Hard
  * Goal: Keep track of disequality relations.
  */
class DisequalityAnalysis extends Analysis {
  // Tag to restrict usability
  trait DisequalityTag

  // Type alias for simplification below
  type DisequalityAnalysisT = Analysis & DisequalityTag {
    type Data = Set[EClass.Id]
    val eclass_data: MutableMap[EClass.Id, Set[EClass.Id]]
  }

  /**
    * [[Extension]], using the type of the analysis, featuring [[user-callable]] functions to access the analysis.
    */
  // object DisequalityAnalysisExtensions {
  //   // Uncomment in case EGraph is used inside extension
  //   import EGraph._
    
  //   extension (analysis: DisequalityAnalysisT) {
  //     /**
  //       * Goal: Add a disunion between two classes.
  //       * 
  //       * @param id1 first class id
  //       * @param id2 second class id
  //       */
  //     def disunion(id1: EClass.Id, id2: EClass.Id): Unit =
  //       // add id2 to first class
  //       val data1 = analysis.eclass_data.getOrElse(id1, Set.empty[EClass.Id])
  //       val newData1 = data1 + id2
  //       analysis.eclass_data.update(id1, newData1)

  //       // add id1 to second class
  //       val data2 = analysis.eclass_data.getOrElse(id2, Set.empty[EClass.Id])
  //       val newData2 = data2 + id1
  //       analysis.eclass_data.update(id2, newData2)
      
  //     /**
  //      * Goal: Check if the analysis is consistent, i.e, no class forbids itself.
  //      */
  //     def is_consistent(egraph: EGraph): Boolean =
  //       egraph.eclasses.forall(c =>
  //         val ccid = egraph.find(c._1).id
  //         analysis.getData(c._1.id).match
  //           case None => true
  //           case Some(dataSet) => dataSet.forall(fid =>
  //             val fc = egraph.getEClassFromId(fid)
  //             egraph.find(fc).id != ccid
  //         )
  //       )

  //       // possibly more efficient version
  //       // analysis.eclass_data.forall((cid, data) =>
  //       //   data.forall(id =>
  //       //     val fc = egraph.getEClassFromId(id)
  //       //     egraph.find(fc).id != cid
  //       //   )
  //       // )
  //   }
  // }

  /**
    * [[Data]] set as [[Seq<EClass.Id>]] to refer to other classes.
    */
  type Data = Set[EClass.Id]
  val eclass_data = MutableMap()

  /**
    * Goal: Set sequence to empty.
    * 
    * @param g graph
    * @param x node
    * @return Seq[EClass.Id]
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    val xc = egraph.find(EClass(x))
    val emptySet: Set[EClass.Id] = Set.empty[EClass.Id]
    eclass_data.update(xc.id, emptySet)

    return emptySet
  }

  /**
    * Goal: Concatenate sequences.
    * 
    * @param data1 Seq of EClass.Id
    * @param data2 Seq of EClass.Id
    * @return Seq[EClass.Id] as the concatenation of data1 and data2
    */
  def merge(data1: Data, data2: Data): Data = {
    val mergedSet = data1 ++ data2
    return mergedSet
  }

  /**
    * Goal: Empty
    * 
    * @note This function could be used ot check for contradictions, but that would be expected to be
    * computationally intensive, therefore it will be implemented as a user-callable instead, check 
    * DisequalityAnalysisExtensions.is_consistent for more information.
    * 
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    return
  }

  /**
    * Goal: Add a disunion between two classes.
    * 
    * @param id1 first class id
    * @param id2 second class id
    */
  def disunion(id1: EClass.Id, id2: EClass.Id): Unit = {
    // add id2 to first class
    val data1 = eclass_data.getOrElse(id1, Set.empty[EClass.Id])
    val newData1 = data1 + id2
    eclass_data.update(id1, newData1)

    // add id1 to second class
    val data2 = eclass_data.getOrElse(id2, Set.empty[EClass.Id])
    val newData2 = data2 + id1
    eclass_data.update(id2, newData2)
  }

  /**
   * Goal: Check if the analysis is consistent, i.e, no class forbids itself.
   */
  def is_consistent[G](egraph: G)(using EGraphOps[G]): Boolean = {
    egraph.eclasses.forall(c =>
      val ccid = egraph.find(c._1).id
      getData(c._1.id).match
        case None => true
        case Some(dataSet) => dataSet.forall(fid =>
          val fc = egraph.getEClassFromId(fid)
          egraph.find(fc).id != ccid
      )
    )
  }
}
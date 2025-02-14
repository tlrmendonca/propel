package propel.evaluator.egraph

import propel.evaluator.egraph.{EClass, ENode, Language}
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet}
import propel.evaluator.egraph.EClass.Id

/**
 * A tool to analyse data in an egraph in parallel with egraph operations.
 */
trait Analysis:
  /**
    * The [[Data]] type pertaining to this [[Analysis]]'s [[EClass]]es.
    */
  type Data;
  val eclass_data: MutableMap[EClass.Id, Data];
  /**
    * A group of operations the [[Analysis]] can use to interpret [[EClass]]es or [[ENode]]s.
    */
  val operations: MutableMap[Operator, (Function1[Seq[Data], Data], Int)] = MutableMap.empty;
  
  /**
    * The [[GlobalData]] of the [[Analysis]] itself, i.e. events.
    */
  type GlobalData;
  var global_data: GlobalData;

  /**
    * A list of [[Analysis]]s that this [[Analysis]] depends on, i.e. need to have run before it.
    * 
    * @note An [[Analysis]] is never responsible for handling data or calling functions of the its dependencies, nor
    * vice-versa. Dependencies exist to provide access to other [[Analysis]]'s data.
    */
  var dependencies: List[Analysis];
  
  def getData(id: EClass.Id): Option[Data] = eclass_data.get(id)

  def setData(id: EClass.Id, data: Data): Unit = eclass_data.update(id, data)

  def deleteData(id: EClass.Id): Unit = eclass_data.remove(id)

  /**
    * Makes a new [[Analysis]]'s data, given an [[ENode]].
    * 
    * @param egraph the specified [[Egraph]].
    * @param enode the specified [[ENode]].
    * @return the data of the new [[EClass]].
    * 
    * @note This function is NOT responsible for adding the [[ENode]], NOR STORING the [[Data]].
    * It should be called in the process of adding an [[ENode]] to an [[EGraph]].
    * @note This function expects given [[ENode]] to be cannonical of its [[EClass]].
    * For more information, refer to [[EGraph.add]]. This implies the client can search
    * the graph to find the [[EClass]] of the given [[ENode]] safely.
    */
  def make[G](egraph: G, enode: ENode)(using EGraphOps[G]): Data;

  /**
    * Defines how to merge two datas.
    *
    * @param data1 the data of the first EClass.
    * @param data2 the data of the second EClass, to be merged.
    * @return the merged data.
    */
  def merge(data1: Data, data2: Data): Data;

  /**
    * An optional function that modifies the given [[EClass]].
    *
    * @param egraph the specified [[Egraph]].
    * @param id an [[EClass]]'s Id.
    * 
    * @note This function must be idempotent, i.e. modify(modify()) = 
    * modify(). Usually adds an [[ENode]] to the given [[EClass]].
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit;

// object AnalysisUtils:

//   /**
//     * A static method to connect two [[Analysis]]s.
//     * 
//     * @param i_analysis the independent [[Analysis]].
//     * @param builder a function that takes the independent [[Analysis]] and returns an analysis dependent on it.
//     */
//   def dependsOn(i_analysis: Analysis, builder: Analysis => Analysis): Analysis = 
//     val d_analysis = builder(i_analysis)

//     new Analysis {
//       def make[G](egraph: G, enode: ENode)(using EGraphOps[G]): Unit = 
//         val i_data = i_analysis.make(egraph, enode)
//         i_analysis.setData(egraph.find(EClass(enode)).id, i_data)
//         val d_data = d_analysis.make(egraph, enode)
//         d_analysis.setData(egraph.find(EClass(enode)).id, d_data)

//       def merge(data1: Data, data2: Data): Data = {}
//         // i_analysis.merge(data1, data2)
//         // d_analysis.merge(data1, data2)

//       def modify[G](egraph: G, id: Id)(using EGraphOps[G]): Unit = 
//         i_analysis.modify(egraph, id)
//         d_analysis.modify(egraph, id)
//     }

      
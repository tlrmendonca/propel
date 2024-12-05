package propel.evaluator.egraph

import propel.evaluator.egraph.{EClass, ENode, Language}
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

/**
 * An tool to analyse data in an egraph in parallel with egraph operations.
 * @note TODO: WIP
 */
trait Analysis:
    type Data;
    val eclass_data: MutableMap[EClass.Id, Data];
    val operations: MutableHashMap[Operator, (Function1[Seq[Data], String], Int)] = MutableHashMap.empty;

    def getData(id: EClass.Id): Option[Data] = eclass_data.get(id)

    def setData(id: EClass.Id, data: Data): Unit = eclass_data.update(id, data)

    // TODO: getOrMakeData(class.id)

    /**
      * Makes a new [[Analysis]]'s data, given an [[ENode]].
      * 
      * @param egraph the specified [[Egraph]].
      * @param enode the specified [[ENode]].
      * @param operations an optional map of operations, mapping [[Operator]]s to functions, to be
      * used in the process of making the [[Data]]
      * @return the data of the new [[EClass]].
      * 
      * @note This function is not responsible for adding the [[ENode]], nor storing the [[Data]].
      * It should be called in the process of adding an [[ENode]] to an [[EGraph]].
      * @note This function expects given [[ENode]] to be cannonical of its [[EClass]].
      * For more information, refer to [[EGraph.add]]. This implies the client can search
      * the graph to find the [[EClass]] of the given [[ENode]] safely.
      */
    def make[A <: Analysis, G[_ <: A]](egraph: G[A], enode: ENode)(using EGraphOps[A, G]): Data;

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
    def modify[A <: Analysis, G[_ <: A]](egraph: G[A], id: EClass.Id)(using EGraphOps[A, G]): Unit;
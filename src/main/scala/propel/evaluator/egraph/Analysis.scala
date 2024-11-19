package propel.evaluator.egraph

import propel.evaluator.egraph.{EClass, ENode, Language}
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet}

/**
 * An tool to analyse data in an egraph in parallel with egraph operations.
 * @note TODO: WIP
 */
trait Analysis:
    type Data;
    val eclass_data: MutableMap[EClass.Id, Data];

    /**
      * Makes a new [[Analysis]]'s data, given an [[ENode]].
      * 
      * @param egraph the specified [[Egraph]].
      * @param enode the specified [[ENode]].
      * 
      * @note This function is not responsible for adding the [[ENode]].
      * It should be called in the process of adding an [[ENode]] to an [[EGraph]].
      * @note This function expects given [[ENode]] to be cannonical of its [[EClass]].
      * For more information, refer to [[EGraph.add]]. 
      */
    def make[A <: Analysis, G[_ <: A]](egraph: G[A], enode: ENode)(using EGraphOps[A, G]): Unit;

    /**
      * Defines how to merge two [[Data]]s, given two [[EClass]]es' Id.
      *
      * @param id1 the id of the first EClass.
      * @param id2 the id of the second EClass, to be merged.
      * 
      * @note TODO: This function would benefit from returning some type
      * of structure indicating the success or failure of this operation,
      * but for now it is not implemented. This idea is inspired by egg's
      * approach.
      * @note This function may be able to affect the [[Analysis]], in
      * the sense that [[modify]] has access to the egraph too. E.g.:
      * storing some data for [[modify]] to process later.
      */
    def merge(id1: EClass.Id, id2: EClass.Id): Unit;

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
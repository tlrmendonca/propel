package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

/**
 * [[Counting Nodes in Classes]]
 * Level: Easy
 * Goal: Keep track of what nodes exist in each class
 * 
 * [[Tracking Nodes in Classes]]
 * Level: Easy
 * Goal: Keep track of what nodes exist in each class and how many nodes are in each class
 */
class CountingNodesAnalysis extends Analysis {

  /**
    * [[Data]] set as [[String]] to use [[ENode]]'s [[Operator]] as a constant value.
    */
  type Data = (Int, Seq[ENode])
  val eclass_data = MutableMap()

  type GlobalData = Unit
  var global_data = ()

  val dependencies = List()

  /**
    * Goal: Set data (number of nodes in class) to 1.
    * 
    * Goal2: Set data to (1, node).
    *
    * @param g graph
    * @param x node
    * @return Tuple (1, node)
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    val xc = egraph.find(EClass(x))
    eclass_data.update(xc.id, (1, Seq(x)))

    return (1, Seq(x))
  }

  /**
    * Goal: Add data of two classes.
    * 
    * Goal2: Merge data of two classes by adding the number of nodes and concatenating the sequences.
    * @note Sequences will never have nodes in common, since that would imply duplication of nodes.
    *
    * @param data1 data
    * @param data2 data
    * @return Tuple (data1._1 + data2._1, data1._2 ++ data2._2)
    */
  def merge(data1: Data, data2: Data): Data = {
    println(s"Merging $data1 and $data2")
    return (data1._1 + data2._1, data1._2 ++ data2._2)
  }

  /**
    * Goal: Empty
    * 
    * Goal2: Empty
    * @note This function is not necessary, but could be used for checking invariants if necessary.
    *
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    return
  }
}
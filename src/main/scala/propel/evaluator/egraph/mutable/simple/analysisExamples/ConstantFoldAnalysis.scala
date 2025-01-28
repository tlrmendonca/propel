package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import java.util.HashMap

/**
  * [[Constant Folding]]
  * Level: Medium
  * Goal: Assert that all enodes deemed equal have the same constant value
  */
class ConstantFoldAnalysis extends Analysis {
  /**
    * [[Data]] set as [[String]] to use [[ENode]]'s [[Operator]] as a constant value.
    */
  type Data = String
  val eclass_data = MutableMap()

  /**
    * Goal: Calculate the constant value if dependent on children.
    * @note Let us assume a node can only have one children.
    *
    * @param g graph
    * @param x node
    * @return constant value of said node
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    // define data
    var cdata = ""
    if !x.refs.isEmpty then
      cdata = getData(x.refs.head.id).get
    var data = x.op.toString + cdata

    // (deprecated)
    // exception - if op is y, then its x (simulating x is y conceptually)
    // this is a *shortcut* to writting a decent example
    // if data == "y" then data = "x"
    
    val xc = egraph.find(EClass(x))
    eclass_data.update(xc.id, data)

    println(s"Make: class ${xc} added and data set to ${eclass_data(xc.id)}")
    return data
  }

  /**
    * Goal: Assert two classes have the same constant value.
    *
    * @param data1 data
    * @param data2 data
    * @return
    */
  def merge(data1: Data, data2: Data): Data = {
    if (data1 == data2) 
      println(s"Merge: $data1 and $data2 are equal")
    else 
      println(s"Merge: Inconsistent data! Shouldn't merge ($data1) and ($data2)")  
    data1
  }

  /**
    * Goal: "Concretize" the constant value into a node, i.e. add a node to the class.
    *
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    val data = getData(id).get
    val c = egraph.getEClassFromId(id)
    val n = ENode(Operator(data))
    val nc = egraph.add(n)
    egraph.union(c, nc)
  }
} 
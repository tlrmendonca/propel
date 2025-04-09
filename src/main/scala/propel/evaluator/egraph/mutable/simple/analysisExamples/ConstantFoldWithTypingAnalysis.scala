package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import java.util.HashMap

/**
  * [[Constant Folding]]
  * Level: Medium
  * Goal: Assert that all enodes deemed equal have the same constant value keeping track of types.
  */
class ConstantFoldWithTypingAnalysis(t_analysis: TypeFoldAnalysis) extends Analysis {
  /**
    * [[Data]] set as [[String]] to use [[ENode]]'s [[Operator]] as a constant value.
    */
  type Data = String
  val eclass_data = MutableMap()

  type GlobalData = Boolean
  var global_data = false

  val dependencies = List(t_analysis)

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
    
    val xc = egraph.find(EClass(x))
    eclass_data.update(xc.id, data)

    val t_data = t_analysis.getData(xc.id).get

    println(s"Make: class ${xc} set as ${eclass_data(xc.id)}, of type ${t_data}")
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
    // for testing purposes:
    if t_analysis.global_data then
      println("Merge: Event detected on dependency analysis (TypeFoldAnalysis)")
    if !(data1 == data2) || t_analysis.global_data then
      global_data = true
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
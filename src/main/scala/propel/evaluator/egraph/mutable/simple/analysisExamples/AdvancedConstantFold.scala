package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import java.util.HashMap

/**
  * [[Advanced Constant Folding]]
  * Level: Hard
  * Goal: Assert that all enodes deemed equal have the same constant value
  */
class AdvConstantFoldAnalysis extends Analysis {
  /**
    * [[Data]] set as [[String]] to use [[ENode]]'s [[Operator]] as a constant value.
    */
  type Data = String
  val eclass_data = MutableMap()

  type GlobalData = Unit
  var global_data = ()

  var dependencies = List()

  /**
    * Goal: Calculate the constant value if dependent on children.
    *
    * @param g graph
    * @param x node
    * @return constant value of said node
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    
    val f = operations.getOrElse(x.op, null)
    val args = x.refs.map(ref => getData(egraph.find(ref).id).get)
    var data = ""
    f match {
      case null => return x.op.toString
      case f: (Function1[Seq[String], String], Int) =>
        if (args.length != f._2) throw new Exception("Invalid number of arguments, expected " + f._2 + ", but " + args.length + " given")
        data = f._1(args)
    }
      
    val xc = egraph.find(EClass(x))
    eclass_data.update(xc.id, data)

    // println(s"Make: class ${xc} added and data set to ${eclass_data(xc.id)}")
    return data
    // return ""
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
      println(s"Inconsistent data! Cannot merge ($data1) and ($data2)")  
    data1
  }

  /**
    * Goal: "Concretize" the constant value into a node, i.e. add a node to the class.
    *
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    val original_class = egraph.getEClassFromId(id)
    val data = getData(id).get
    val new_node = ENode(Operator(data)) // concretize
    val new_class = egraph.add(new_node)
    egraph.union(original_class, new_class) // this joins any two classes with the same data
  }

  val functions = MutableHashMap[Operator, (Function1[Seq[String], String], Int)](
    Operator("+") -> (args => ((args(0).toInt + args(1).toInt).toString), 2),
    Operator("sub") -> (args => ((args(0).toInt - args(1).toInt).toString), 2),
    Operator("mul") -> (args => ((args(0).toInt * args(1).toInt).toString), 2),
    Operator("div") -> (args => ((args(0).toInt / args(1).toInt).toString), 2),
    Operator("pow2") -> (args => ((args(0).toInt * args(0).toInt).toString), 1),
  )

  // Functions need to be added since operations is unmodifiable
  operations ++= functions
}
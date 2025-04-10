package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps, AnalysisType, BType}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

/**
  * [[Type Folding]]
  * Level: Medium
  * Goal: Assert that all enodes respect typing relations
  */
class TypeFoldAnalysis extends Analysis {
  /**
    * [[Data]] set as [[String]] to simplify representation of types.
    */
  type Data = AnalysisType
  val eclass_data = MutableMap()

	/**
	 * [[GlobalData]] set as [[Boolean]] to represent the presence of an error/inconsistency.
	 */
  type GlobalData = Boolean
  var global_data = false

  val dependencies = List()


  /**
    * Goal: Represent the type of a node.
    *
    * @param g graph
    * @param x node
    * @return type of said node
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    val f = operations.getOrElse(x.op, null)
    val args = x.refs.map(ref => getData(ref.id).get)
    f match {
      case null => AnalysisType(toType(x.op)) // TODO: distinguish a value from a non-defined function symbol ?
      case f: (Function1[Seq[AnalysisType], AnalysisType], Int) =>
        if (args.length != f._2) {
          global_data = true
          println("WARNING: Invalid number of arguments, expected " + f._2 + ", but " + args.length + " given")
        }
        f._1(args)
    }
  }

  /**
    * Goal: Auxiliary function to define the type of a basic [[ENode]].
    * 
    * @note "Basic" in this context refers to nodes that are not dependent on children, i.e., numbers, strings and booleans.
    *
    * @param op operator
    * @return type of said operator
    */
  private def toType(op: Operator): BType = {
    op.toString match {
      case "true" | "false" => BType.Boolean
      case s if s.matches("""-?\d+(\.\d+)?""") => BType.Number
      case _ => BType.String
    }
  }

  /**
    * Goal: check if two classes have the same type.
    *
    * @param data1 data
    * @param data2 data
    * @return
    */
  def merge(data1: Data, data2: Data): Data = {
    if (data1 == data2) 
      println(s"Merge: $data1 and $data2 are the same type")
    else {
      global_data = true
      println(s"WARNING: Inconsistent types! Cannot merge (${data1.toString()}) and (${data2.toString()})") 
    }
    data1
  }

  /**
    * Goal: Nothing. Can be used for checks, but in such a simple analysis those can be made in *make* and *merge* without overhead.
    *
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    return
  }
  
  // TODO: there operators are very complicated and could use a refactor
  // Something that would put emphasis on the logic part and leave the warnings handling in another place
  val functions = MutableHashMap[Operator, (Function1[Seq[AnalysisType], AnalysisType], Int)] (
    Operator("+") -> (args => {
      if !(args(0).basicType == BType.Number && args(1).basicType == BType.Number)
      then {
        global_data = true
        println(s"WARNING: Invalid types for +, given (${args(0)}) and (${args(1)})")
      }
      AnalysisType(BType.Number)
      },2),
    Operator("add1") -> (args => {
      if !(args(0).basicType == BType.Number)
      then {
        global_data = true
        println(s"WARNING: Invalid types for add1, given (${args(0)})")
      }
      AnalysisType(Seq(AnalysisType(BType.Number)), AnalysisType(BType.Number))
    },1)
  )
      
  operations ++= functions
}
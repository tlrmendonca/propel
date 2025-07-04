package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps, AnalysisType, LType}
import propel.evaluator.egraph.mutable.simple.Op.* 
import propel.evaluator.egraph.mutable.simple.Expr.* 

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
  type Data = LType
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
    // Check if var
    val t : LType = resolveVar(x.op.toString)
    if (t != Never) return t

    val f = operations(Op.fromString(x.op.toString))
    val args : Seq[LType] = x.refs.map(ref => {
      val cRef = egraph.find(ref)
      getData(cRef.id).get
    })
    f match {
      case Never => toType(x.op)
      case f: Function1[Seq[LType], LType] => f(args)
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
  private def toType(op: Operator): LType = {
    op.toString match {
      case "true" | "false" => LType.Boolean
      case s if s.matches("""-?\d+(\.\d+)?""") => LType.Number
      case s if s.matches("""\(\d+(,\d+)*\)""") => LType.List // lists looking like (1,2,3) or (42) // HERE
      case _ => LType.String
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
    if !(data1 == data2) then {
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

  def printWarning(msg: String): Unit = {
    global_data = true
    println(s"WARNING: $msg")
  }
  
  // val functions = MutableHashMap[Operator, (Function1[Seq[AnalysisType], AnalysisType], Int)] (
  //   Operator("+") -> (args => {
  //     if !(args(0).basicType == args(1).basicType && (args(0).basicType == LType.Number || args(0).basicType == LType.String)) then printWarning(s"Invalid types for +, given (${args(0)}) and (${args(1)})")
  //     AnalysisType(args(0).basicType)
  //     },2),
  //   Operator("*") -> (args => {
  //     if !(args(0).basicType == LType.Number && args(1).basicType == LType.Number) then printWarning(s"Invalid types for *, given (${args(0)}) and (${args(1)})")
  //     AnalysisType(LType.Number)
  //     },2),
  //   Operator("add1") -> (args => {
  //     if !(args(0).basicType == LType.Number) then printWarning(s"Invalid types for add1, given (${args(0)})")
  //     AnalysisType(Seq(AnalysisType(LType.Number)), AnalysisType(LType.Number))
  //   },1),
  //   Operator("pow2") -> (args => {
  //     if !(args(0).basicType == LType.Number) then printWarning(s"Invalid types for pow2, given (${args(0)})")
  //     AnalysisType(LType.Number)
  //   },1),
  // )

  // val var_types = MutableHashMap[Operator, (Function1[Seq[AnalysisType], AnalysisType], Int)] (
  //   Operator("x") -> (args => AnalysisType(LType.Number),0),
  //   Operator("y") -> (args => AnalysisType(LType.Number),0),
  //   Operator("s") -> (args => AnalysisType(LType.String),0),
  //   Operator("v") -> (args => AnalysisType(LType.String),0),
  //   Operator("l") -> (args => AnalysisType(LType.List),0),
  // )

  override def operations(op: Op) : Function1[Seq[LType], LType] = op match {
    case PLUS => args => {args match {
      case Seq(LType.Number, LType.Number) => LType.Number
      case Seq(LType.String, LType.String) => LType.String
      case Seq(LType.List(of), LType.List(of2)) if of == of2 => LType.List(of)
      case _ => printWarning(s"Invalid types for +, given (${args.mkString(", ")})"); Never
    }}
    case MINUS => args => {args match {
      case Seq(LType.Number, LType.Number) => LType.Number
      case _ => printWarning(s"Invalid types for -, given (${args.mkString(", ")})"); Never
    }}
    case MULT => args => {args match {
      case Seq(LType.Number, LType.Number) => LType.Number
      case _ => printWarning(s"Invalid types for *, given (${args.mkString(", ")})"); Never
    }}
    case UNKNOWN => args => {
      printWarning(s"Unknown operator: $op with args (${args.mkString(", ")})")
      Never
    }
  }

  private def resolveVar(name: String): LType = name match {
    case "x" | "y" => LType.Number
    case "s" | "v" => LType.String
    case "l" => LType.List(of: LType.Number) // assuming lists of numbers for now
    case _ => Never
  }
      
  operations ++= functions
}
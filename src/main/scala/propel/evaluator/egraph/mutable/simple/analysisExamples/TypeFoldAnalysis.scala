package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps, Op, LType}
import propel.evaluator.egraph.mutable.simple.LType.*
import propel.evaluator.egraph.mutable.simple.Op.*
import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

/**
  * [[Type Folding]]
  * Level: Medium
  * Goal: Assert that all enodes respect typing relations
  */
class TypeFoldAnalysis(varsAnalysis: VarsAnalysis) extends Analysis {
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

  val dependencies = scala.List(varsAnalysis)

  /**
    * Goal: Represent the type of a node.
    *
    * @param g graph
    * @param x node
    * @return type of said node
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    // Check if var
    val xc = egraph.find(EClass(x))
    val xc_var = varsAnalysis.getData(xc.id).get // data is Boolean
    
    if (xc_var) return varsAnalysis.varTypes.getOrElse(x.op.toString(), Never)

    // If not var, check if operation
    val fo = operations(Op.fromString(x.op.toString))
    if (fo.isEmpty) return toType(x.op) // basic type (number, string, boolean)
    
    // Run operation/function
    val f = fo.get
    val args : Seq[LType] = x.refs.map(ref => {
      val cRef = egraph.find(ref)
      getData(cRef.id).get
    })
    return f(args)
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
    val res = op.toString match {
      case "true" | "false" => LType.Boolean
      case s if s.matches("""-?\d+(\.\d+)?""") => LType.Number
      case s if s.matches("""\(\d+(,\d+)*\)""") => LType.List(Never) // lists looking like (1,2,3) or (42) // HERE
      case _ => LType.String
    }
    println("Basic type detected for operator " + op.toString + " : " + res.toString())
    return res
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
  
  override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None) : Option[Function1[Seq[LType], LType]] = op match {
    case PLUS => Some(args => {args match {
      case Seq(LType.Number, LType.Number) => LType.Number
      case Seq(LType.String, LType.String) => LType.String
      case Seq(LType.List(of), LType.List(of2)) if of == of2 => LType.List(of)
      case _ => printWarning(s"Invalid types for +, given (${args.mkString(", ")})"); Never
    }})
    case MINUS => Some(args => {args match {
      case Seq(LType.Number, LType.Number) => LType.Number
      case _ => printWarning(s"Invalid types for -, given (${args.mkString(", ")})"); Never
    }})
    case MULT => Some(args => {args match {
      case Seq(LType.Number, LType.Number) => LType.Number
      case _ => printWarning(s"Invalid types for *, given (${args.mkString(", ")})"); Never
    }})
    case DIV => Some(args => {args match {
      case Seq(LType.Number, LType.Number) => LType.Number
      case _ => printWarning(s"Invalid types for /, given (${args.mkString(", ")})"); Never
    }})
    case POW2 => Some(args => {args match {
      case Seq(LType.Number) => LType.Number
      case _ => printWarning(s"Invalid types for ^, given (${args.mkString(", ")})"); Never
    }})
    case SQRT => Some(args => {args match {
      case Seq(LType.Number) => LType.Number
      case _ => printWarning(s"Invalid types for sqrt, given (${args.mkString(", ")})"); Never
    }})
    case UNKNOWN => None
  }

  private def resolveVar(name: String): LType = name match {
    case "x" | "y" => LType.Number
    case "s" | "v" => LType.String
    case "l" => LType.List(LType.Number) // assuming lists of numbers for now
    case _ => Never
  }
}
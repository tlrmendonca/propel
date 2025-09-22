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

  val dependencies = scala.List()

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
    if (t != LType.Never) return t

    val f = operations(Op.fromString(x.op.toString))
    val args : Seq[LType] = x.refs.map(ref => {
      val cRef = egraph.find(ref)
      getData(cRef.id).get
    })
    f match {
      case LType.Never => toType(x.op) // FIXME: unreachable case, cuz f is a function type a -> b, id like to have this branch trigger if "b" is Never
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
      case s if s.matches("""\(\d+(,\d+)*\)""") => LType.List(Never) // lists looking like (1,2,3) or (42) // HERE
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
  
  override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None) : Function1[Seq[LType], LType] = op match {
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
    case UNKNOWN | _ => args => {
      printWarning(s"Unknown operator: $op with args (${args.mkString(", ")})")
      Never
    }
  }

  private def resolveVar(name: String): LType = name match {
    case "x" | "y" => LType.Number
    case "s" | "v" => LType.String
    case "l" => LType.List(LType.Number) // assuming lists of numbers for now
    case _ => Never
  }
}
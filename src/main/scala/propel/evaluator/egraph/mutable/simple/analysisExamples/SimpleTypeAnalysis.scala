package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.simple.*
import propel.evaluator.egraph.mutable.simple.Type.*
import propel.evaluator.egraph.mutable.simple.ConstructorName.*
import collection.mutable.{Map as MutableMap}

/**
  * [[TypeAnalysis]]
  * Goal: Track types of expressions using the SimpleLanguage definitions.
  */
class TypeAnalysis(varsAnalysis: SimpleVarsAnalysis) extends Analysis {
  /**
    * [[Data]] set as [[Type]] from SimpleLanguage.
    */
  type Data = Type
  val eclass_data: MutableMap[EClass.Id, Data] = MutableMap()

  /**
    * [[GlobalData]] set as [[Boolean]] to represent inconsistent states.
    */
  type GlobalData = Boolean
  var global_data: GlobalData = false

  val dependencies = scala.List(varsAnalysis)

  /**
    * Goal: Determine the type of an [[ENode]].
    *
    * @param egraph graph
    * @param x node
    * @return type of said node
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    val opStr = x.op.toString()

    // 1. Check if it's a variable
    val xc = egraph.find(EClass(x))
    varsAnalysis.getData(xc.id).flatten match {
      case Some(t) => return t
      case None => ()
    }

    // 2. Check if it's a known constructor
    try {
      val cName = ConstructorName.valueOf(opStr)
      constructor_type(cName) match {
        case Function(_, ret) => return ret
        case t => return t
      }
    } catch {
      case _: IllegalArgumentException => ()
    }

    // 3. Check if it's a known function
    function_types.get(opStr) match {
      case Some(Function(_, ret)) => return ret
      case Some(t) => return t
      case None => ()
    }

    // Default to Nat if unknown
    Nat
  }

  /**
    * Goal: check if two classes have the same type.
    *
    * @param data1 data
    * @param data2 data
    * @return the resulting data
    */
  def merge(data1: Data, data2: Data): Data = {
    if (data1 != data2) {
      global_data = true
      println(s"ERROR: Type mismatch! Cannot merge ${data1} with ${data2}")
    }
    data1
  }

  /**
    * Goal: Optional transformation.
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = ()

  /**
    * Goal: Mapping [[Op]] to type transformations.
    */
  override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None): Option[Function1[Seq[Type], Type]] = {
    val opStr = op.toString()

    // Constructor logic
    try {
      val cName = ConstructorName.valueOf(opStr)
      val t = constructor_type(cName)
      return Some(_ => t match {
        case Function(_, ret) => ret
        case other => other
      })
    } catch {
      case _: IllegalArgumentException => ()
    }

    // Function logic
    function_types.get(opStr) match {
      case Some(Function(_, ret)) => Some(_ => ret)
      case Some(t) => Some(_ => t)
      case None => None
    }
  }
}

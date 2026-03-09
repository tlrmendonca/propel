package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.*
import propel.evaluator.egraph.mutable.simple.Type.*
import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import propel.evaluator.egraph.EClass.Id
import propel.evaluator.egraph.ENode
import scala.compiletime.ops.string

/**
  * [[]]
  * Goal: Identify nodes that represent variables.
  */
class ExprExtractorAnalysis(varList: MutableHashMap[String, Type] = MutableHashMap()) extends Analysis {
  /**
    * [[Data]] set as [[String]].
    */
  type Data = (String, Map[ENode.Id, Int])
  val eclass_data = MutableMap()

    /**
     * [[GlobalData]] set as [[Boolean]] though it is irrelevant here.
     */
  type GlobalData = Boolean
  var global_data = false

  val dependencies = scala.List()

  /**
    * Goal: Assign string representation to class.
    *
    * @param g graph
    * @param x node
    * @return Eclass expression.
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    // return a string like "op()" with all the children's representations inside like "op(c1, c2, ...)"
    // unless it has no children, in which case just "op"
    var expr : String = ""
    var childrenDepths: MutableMap[ENode.Id, Int] = MutableMap()
    if (x.refs.isEmpty)
      expr = x.op.toString()
      childrenDepths(x.id) = 0 // depth is 0 for leaves
    else 
      expr = x.op.toString + "(" + x.refs.map(ref => this.getData(ref.id).get._1).mkString(",") + ")" // TODO: test
      childrenDepths(x.id) = 1 + x.refs.map(ref => this.getData(ref.id).get._2.values.min).max // depth is 1 + max depth of children 
    return (expr, childrenDepths.toMap)
  }

  /**
    * Goal: Choose a representation based on merging classes' depths.
    *
    * @param data1 data
    * @param data2 data
    * @return
    */
  def merge(data1: Data, data2: Data): Data = {
    // choose the expression with the minimum depth
    val (expr1, depths1) = data1
    val (expr2, depths2) = data2

    // find minimum depth in both
    val minDepth1 = if (depths1.isEmpty) Int.MaxValue else depths1.values.min
    val minDepth2 = if (depths2.isEmpty) Int.MaxValue else depths2.values.min

    // keep the expression we believe is simpler (minimum depth)
    if (minDepth1 <= minDepth2) {
      return data1
    } else {
      return data2
    }
  }
  
  /**
    * Goal: Empty.
    *
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    return
  }

  // NOTE: This is a hack to get rid of an error, although this could be used to define custom formatting for example.
  override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None): Option[Function1[Seq[Data], Data]] = {
    op match {
      case _ => None
    }
  }
}
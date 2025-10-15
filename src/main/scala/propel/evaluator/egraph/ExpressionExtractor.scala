package propel.evaluator.egraph

import propel.evaluator.egraph.{EClass, ENode, Language}
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps, Op}
import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}


/** A class to extract an Expression from an [[EClass]] */
object ExpressionExtractor {
  
  /**
   * Extract an expression from an [[EClass]] by recursively making a choice between the referenced
   * [[ENode]]s which have the least divergent path, i.e. prefer childless nodes, but choose randomly between
   * [[ENode]]s with the same number of children.
   * 
   * @param g the specified [[EGraph]].
   * @param eclass the specified [[EClass]].
   * @return an expression in string format.
   */
  def extract[G](g: G, eclass: EClass)(using EGraphOps[G]) : String = {
    // Initialize a map of eclass ids only once
    val eclass_map = g.eclasses

    def helper(eclass: EClass): String = {
      // enodes for this eclass
      val enodes : Set[ENode] = eclass_map(eclass)

      // find the ones with the least children
      val min_children : Int = enodes.map(_.refs.size).min
      val min_children_nodes : Seq[ENode] = enodes.filter(_.refs.size == min_children).toSeq

      // choose one randomly
      val chosen_node : ENode = min_children_nodes(scala.util.Random.nextInt(min_children_nodes.size))

      // return a string like "op()" with all the children ops inside like "op(cop1(...), cop2(...), ...)"
      // unless it has no children
      if (chosen_node.refs.isEmpty) return chosen_node.op.toString()
      else return chosen_node.op.toString + "(" + chosen_node.refs.map(ref => helper(ref)).mkString(",") + ")"
    }
    return helper(eclass)
  }
}

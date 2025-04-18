package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

/**
  * [[Characteristic Vectors]]
  * Goal: Associate with each class a list of values that depend on its connection to other classes.
  * E.g.: x is assigned the randomly generated list [0, 3, 25, 100], then 2x is assigned the list [0, 6, 50, 200].
  * This allows a quick way to prove inequalities and efficient pruning of the lemma candidates space.
  */
class CVecAnalysis extends Analysis {
  /**
    * [[Data]] set as [[Seq<EClass.Id>]] to refer to other classes.
    */
  type Data = Seq[Int] // set to int for simplicity
  val eclass_data = MutableMap()

  type GlobalData = Unit // not relevant now
  var global_data = ()

  val dependencies = List()

  private var CVEC_SIZE = 10
  private var vars = MutableMap[String, Seq[Int]]()

  val functions = MutableHashMap[Operator, (Function1[Seq[Int], Int], Int)](
    Operator("+") -> (args => args(0) + args(1), 2),
    Operator("-") -> (args => args(0) - args(1), 2),
    Operator("*") -> (args => args(0) * args(1), 2),
    Operator("/") -> (args => args(0) / args(1), 2),
    Operator("pow2") -> (args => args(0) * args(0), 1),
  )

  // operations ++= functions // Note: we are avoiding using this bs but it is what was supposed to happen

  /**
    * Goal: calculate a characteristic vector based on given node.
    * 
    * @param g graph
    * @param x node
    * @return cvec
    */
  def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
    // this is "safe" because make runs when we know the xc is brand new, i.e. no overriding
    // (refer to EGraph.add() for more info)
    val xc = egraph.find(EClass(x))
    
    if(is_var(x)) {
      // add variable x.op to the list of known variables
      vars.update(x.op.toString, generate_cvec()) // TODO: not needed
      eclass_data.update(xc.id, vars(x.op.toString))
      return vars(x.op.toString)
    }

    if(is_const(x)) {
      eclass_data.update(xc.id, Seq.fill(CVEC_SIZE)(x.op.toString.toInt)) // e.g. "2" -> Seq(2, 2, 2, 2, 2, 2, 2, 2, 2, 2) because it always means 2
      return Seq.fill(CVEC_SIZE)(x.op.toString.toInt)
    }

    // lastly: not a var and not a const -> expecting a function from the defined language
    // make sure we have a known function
    val f = functions.getOrElse(x.op, null)
    if (f == null) {
      throw new Exception("Unknown function: " + x.op)
    }

    // onwards to finding calculating the cvec depending on the children
    // build cvec one position at a time by applying function to the possible values of each side
    val children = x.refs
    var cvec = Seq.empty[Int]
    for (i <- 0 until CVEC_SIZE) {
      val args : Seq[Int] = children.map(c => eclass_data.getOrElse(c.id, Seq()).apply(i))
      val v = f._1(args)
      cvec = cvec :+ v
    }
    eclass_data.update(xc.id, cvec)
    return cvec
  }

  private def generate_cvec(): Seq[Int] = {
    val cvec = Seq.fill(CVEC_SIZE)(scala.util.Random.nextInt(20))
    return cvec
  }

  private def is_var(x: ENode): Boolean = {
    val op = x.op.toString
    return (op.length == 1 && op.head.isLetter)
  }

  private def is_const(x: ENode): Boolean = {
    val op = x.op.toString
    return (op.forall(_.isDigit))
  }

  /**
    * Goal: check for contradicting cvecs
    * 
    * @param data1 
    * @param data2 
    * @return data1
    */
  def merge(data1: Data, data2: Data): Data = {
    // check if cvecs are the same
    if (data1 != data2) {
      print("Warning: Merging two different cvecs -> contraction")
    }
    return data1
  }

  /**
    * Goal: Empty
    * TODO: is this true?
    * 
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    return
  }
}
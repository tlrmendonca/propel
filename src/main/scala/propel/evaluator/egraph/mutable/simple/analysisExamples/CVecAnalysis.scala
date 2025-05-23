package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import propel.evaluator.egraph.mutable.simple.analysisExamples.TypeFoldAnalysis
import propel.evaluator.egraph.mutable.simple.BType
import scala.annotation.varargs

/**
  * [[Characteristic Vectors]]
  * Goal: Associate with each class a list of values that depend on its connection to other classes.
  * E.g.: x is assigned the randomly generated list [0, 3, 25, 100], then 2x is assigned the list [0, 6, 50, 200].
  * This allows a quick way to prove inequalities and efficient pruning of the lemma candidates space.
  */
class CVecAnalysis(type_analysis: TypeFoldAnalysis) extends Analysis {
  /**
    * [[Data]] set as [[Seq<EClass.Id>]] to refer to other classes.
    */
  type Data = Seq[String] // HERE
  val eclass_data = MutableMap()

  type GlobalData = Unit // not relevant now
  var global_data = ()

  val dependencies = List(type_analysis)

  private val CVEC_SIZE = 10

  val functions_integers = MutableHashMap[Operator, (Function1[Seq[Int], Int], Int)](
    Operator("+") -> (args => args(0) + args(1), 2),
    Operator("-") -> (args => args(0) - args(1), 2),
    Operator("*") -> (args => args(0) * args(1), 2),
    Operator("/") -> (args => args(0) / args(1), 2),
    Operator("pow2") -> (args => args(0) * args(0), 1),
  )

  val functions_strings = MutableHashMap[Operator, (Function1[Seq[String], String], Int)](
    Operator("+") -> (args => args.mkString(""), 2),
    Operator("concat") -> (args => args.mkString(""), 2),
  )

  val functions_lists = MutableHashMap[Operator, (Function1[Seq[String], String], Int)](
    Operator("concat") -> (args => {
      val strip = (s: String) => s.stripPrefix("(").stripSuffix(")")
      s"(${List(strip(args(1)), strip(args(2))).filter(_.nonEmpty).mkString(",")})"
    }, 2),
    // Operator("zip") -> (args => {
    //   def toSeq(s: String): Seq[String] = Option(s).map(str => str.stripPrefix("(").stripSuffix(")").split(",").toList).getOrElse(List.empty)
    //   toSeq(args(1)).zip(toSeq(args(2))).flatMap{case (a, b) => Seq(a, b)}.mkString("(", ",", ")")
    // }, 2),
  )

  val all_functions = Map(
    BType.Number -> functions_integers,
    BType.String -> functions_strings,
    BType.List -> functions_lists
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
    val xc_type = type_analysis.getData(x.id).get.basicType
    
    if(is_var(x)) {
      // add variable x.op to the list of known variables
      val var_type = type_analysis.getData(x.id).get
      if (var_type.basicType == BType.Function) {
        throw new Exception("Function type not supported for characteristic vector generation")
      }
      val cvec = generate_cvec(var_type.basicType)
      eclass_data.update(xc.id, cvec)
      return cvec
    }

    if(is_const(x)) {
      eclass_data.update(xc.id, Seq.fill(CVEC_SIZE)(x.op.toString)) // e.g. "2" -> Seq(2, 2, 2, 2, 2, 2, 2, 2, 2, 2) because it always means 2
      return Seq.fill(CVEC_SIZE)(x.op.toString)
    }

    // lastly: not a var and not a const -> expecting a function from the defined language

    // onwards to finding calculating the cvec depending on the children
    // build cvec one position at a time by applying function to the possible values of each side
    val children = x.refs
    var cvec = Seq.empty[String]
    for (i <- 0 until CVEC_SIZE) {
      var args : Seq[String] = children.map(c => {
        val cc = egraph.find(c)
        eclass_data.getOrElse(cc.id, Seq()).apply(i)
      }) // inefficient as hell
      // cast arguments
      // FIXME: there is an assumption that operations happen between same-typed args
      xc_type match {
        case BType.Number => {
          val int_args = args.map(_.toInt)
          val f = functions_integers.getOrElse(x.op, null)
          assert(f != null, "Unknown function: " + x.op)
          val v = f._1(int_args)
          cvec = cvec :+ v.toString()
        }
        case BType.String => {
          val f = functions_strings.getOrElse(x.op, null)
          assert(f != null, "Unknown function: " + x.op)
          val v = f._1(args)
          cvec = cvec :+ v.toString()
        }
        case BType.List => {
          val f = functions_lists.getOrElse(x.op, null)
          assert(f != null, "Unknown function: " + x.op)
          val v = f._1(args)
          cvec = cvec :+ v.toString()
        }
        case BType.Boolean => throw new Exception("Boolean type not supported for characteristic vector generation")
        case BType.Function => throw new Exception("Function type not supported for characteristic vector generation")
        case _ => throw new Exception("Unknown type: " + xc_type)
      }
    }
    eclass_data.update(xc.id, cvec)
    return cvec
  }

  private def generate_cvec(t: BType): Seq[String] = {
    t match {
      case BType.Number => return Seq.fill(CVEC_SIZE)(scala.util.Random.nextInt(20).toString())
      case BType.String => return Seq.fill(CVEC_SIZE)(scala.util.Random.alphanumeric.take(5).mkString)
      case BType.List => return Seq.fill(CVEC_SIZE) {
        val size = scala.util.Random.nextInt(3) + 1
        val nums = Seq.fill(size)(scala.util.Random.nextInt(20).toString())
        nums.mkString("(", ", ", ")")
      }
      case _ => throw new Exception("Unknown type: " + t)
    }
    val cvec = Seq.fill(CVEC_SIZE)(scala.util.Random.nextInt(20).toString())
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
    * 
    * @param egraph graph
    * @param id class id
    */
  def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
    return
  }
}
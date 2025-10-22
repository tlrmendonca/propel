package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import propel.evaluator.egraph.mutable.simple.analysisExamples.TypeFoldAnalysis
import propel.evaluator.egraph.mutable.simple.{Op, Expr, Value, LType}
import propel.evaluator.egraph.mutable.simple.Op.*
import propel.evaluator.egraph.mutable.simple.Expr.*
import propel.evaluator.egraph.mutable.simple.Value.*
import scala.annotation.varargs
import propel.dsl.impl.Checked.check

/**
  * [[Characteristic Vectors]]
  * Goal: Associate with each class a list of values that depend on its connection to other classes.
  * E.g.: x is assigned the randomly generated list [0, 3, 25, 100], then 2x is assigned the list [0, 6, 50, 200].
  * This allows a quick way to prove inequalities and efficient pruning of the lemma candidates space.
  */
class CVecAnalysis(
    type_analysis: TypeFoldAnalysis,
    vars_analysis: VarsAnalysis,
    disequality_analysis: DisequalityAnalysis
) extends Analysis {
    /**
      * [[Data]] set as [[Seq<EClass.Id>]] to refer to other classes.
      */
    type Data = Seq[Value] // Value is important because I want my CVECs to not be "simplifiable" anymore
    val eclass_data = MutableMap()

    type GlobalData = Boolean // not relevant now
    var global_data = false

    val dependencies = List(type_analysis, vars_analysis, disequality_analysis)

    private val CVEC_SIZE = 5

    // Helper debug function
    def printWarning(msg: String): Unit = {
        global_data = true
        println(s"WARNING: $msg")
    }

    override def operations(op: Op, children_ids: Option[Seq[EClass.Id]] = None): Option[Function1[Seq[Data], Data]] = {
        // Note: arity and typing are assumed to be correct (checked in TypeFoldAnalysis)
        assert(children_ids.isDefined, printWarning("CVecAnalysis operations called with undefined children_ids"))
        val children_types : Seq[type_analysis.Data] = children_ids.get.map(id => type_analysis.getData(id).get)
        // Use of Seq here is a workaround because I was lazy to do it properly before. A refactor is in order.
        // NOTE: What happens is this operation should work with Data, but I for some reason wrote the call site to call this operation
        // on every 2 elements of data by hand, while what should happen is receiving the full Seq[Data] here and applying it to each
        // pair, trio, whatever the arity is, case by case.
        op match {
            case PLUS => Some((a : Seq[Seq[Value]]) => { val args = a(0); children_types match
                case Seq(LType.Number, LType.Number) => Seq(NumValue(getValueNum(args(0)) + getValueNum(args(1))))
                case Seq(LType.String, LType.String) => Seq(StrValue(getValueStr(args(0)) + getValueStr(args(1))))
                case Seq(LType.List(of), LType.List(of2)) => Seq(ListValue(elements = getElementsList(args(0)) ++ getElementsList(args(1))))
                case _ => printWarning(s"Don't know how to $op with args (${args.mkString(", ")})"); Seq()
            })
            case MINUS => Some((a : Seq[Seq[Value]]) => { val args = a(0); children_types match
                case Seq(LType.Number, LType.Number) => Seq(NumValue(getValueNum(args(0)) - getValueNum(args(1))))
                case _ => printWarning(s"Don't know how to $op with args (${args.mkString(", ")})"); Seq()
            })
            case MULT => Some((a : Seq[Seq[Value]]) => { val args = a(0); children_types match
                case Seq(LType.Number, LType.Number) => Seq(NumValue(getValueNum(args(0)) * getValueNum(args(1))))
                case _ => printWarning(s"Don't know how to $op with args (${args.mkString(", ")})"); Seq()
            })
            case DIV => Some((a : Seq[Seq[Value]]) => { val args = a(0); children_types match
                case Seq(LType.Number, LType.Number) => {
                    val denom = getValueNum(args(1))
                    if (denom == 0) {
                        printWarning(s"Division by zero in $op with args (${args.mkString(", ")})")
                        Seq(NumValue(0)) // arbitrary value
                    } else {
                        Seq(NumValue(getValueNum(args(0)) / denom))
                    }
                }
                case _ => printWarning(s"Don't know how to $op with args (${args.mkString(", ")})"); Seq()
            })
            case POW2 => Some((a : Seq[Seq[Value]]) => { val args = a(0); children_types match
                case Seq(LType.Number) => Seq(NumValue(Math.pow(getValueNum(args(0)), 2)))
                case _ => printWarning(s"Don't know how to $op with args (${args.mkString(", ")})"); Seq()
            })
            case SQRT => Some((a : Seq[Seq[Value]]) => { val args = a(0); children_types match
                case Seq(LType.Number) => Seq(NumValue(Math.sqrt(getValueNum(args(0)))))
                case _ => printWarning(s"Don't know how to $op with args (${args.mkString(", ")})"); Seq()
            })
            case UNKNOWN => Some(args => { printWarning(s"CVEC: Unknown operator: $op with args (${args.mkString(", ")})"); Seq()})
        }
    }

    private def check_type(xc_type: LType, op: Operator) : Value = {
      xc_type match {
        case LType.Number => NumValue(op.toString.toDouble)
        case LType.String => StrValue(op.toString)
        case LType.Boolean => BoolValue(op.toString.toBoolean)
        // Special case -> op is a "string" that represents a list, e.g. "(1,2,3)" so we need to do some string manipulation
        case LType.List(of) => {
          val els = op.toString.stripPrefix("(").stripSuffix(")").split(",").nn.toSeq
          val els_ops = els.map(s => Operator(s.nn)) // convert to individual operators
          ListValue(els_ops.map(o => check_type(of, o))) // make a sequence where each element will check its type
          // Note: this is hacky and innefficient. We know lists are homogeneous, so we could just check the type of the first element
        }
        case _ => throw new Exception("Unknown type in cvec_analysis: " + xc_type)
      }
    }

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
        val xc_type = type_analysis.getData(xc.id).get
        // assert(xc_type != Unit, printWarning(s"Type analysis data not found for node: " + x))

        if(is_var(egraph, x)) {
            // add variable x.op to the list of known variables
            if (xc_type == LType.Function) {
                throw new Exception("Function type not supported for characteristic vector generation")
            }
            val cvec = generate_cvec(xc_type)
            eclass_data.update(xc.id, cvec)
            return cvec
        }

        // TODO: improve this check (should be called "is_value", as in, something that is not a name of something). because this being
        // a correct constant check relies on the fact that i comes after a is_var check, leading to only having operations and constants here
        if(is_not_operation(x)) {
            // check type
            val value_of : Value = check_type(xc_type, x.op)

            eclass_data.update(xc.id, Seq.fill(CVEC_SIZE)(value_of)) // e.g. "2" -> Seq(2, 2, 2, 2, 2, 2, 2, 2, 2, 2) because it always means 2
            return Seq.fill(CVEC_SIZE)(value_of)
        }

        // lastly: not var and not const -> expecting an operation between classes (i.e. a function application)

        // build cvec one position at a time by applying the operation to each value from the referenced classes
        val children = x.refs.map(cc => egraph.find(cc)) // canonicalized children
        val children_values = children.map(c => eclass_data.getOrElse(c.id, throw new Exception("No data found for child during cvec generation: " + c.id)))
        var cvec = Seq.empty[Value]
        for (i <- 0 until CVEC_SIZE) {
            // Note: typing is assumed to be correct (checked in TypeFoldAnalysis)
            val args = children_values.map(_.apply(i)) // select i-th of each
            // println(s"Generating cvec for $x, args: ${args.mkString(", ")}")
            val fo = operations(
                Op.fromString(x.op.toString),
                Some(children.map(_.id))
            )
            assert(fo.isDefined, printWarning(s"WARNING: Undefined/unknown function during cvec generation: " + x.op))
            val f = fo.get

            val res = f(Seq(args)) // res should be a Value
            // assert(res(0) != Unit, printWarning(s"Function returned null during cvec generation: " + x.op))
            cvec = cvec :+ res(0)
        }
        eclass_data.update(xc.id, cvec)
        return cvec
    }

    // TODO: pass CVEC_SIZE as a parameter
    private def generate_cvec(t: LType): Seq[Value] = {
        t match {
            case LType.Number => return Seq.fill(CVEC_SIZE)(NumValue(util.Random.between(-20, 21)))
            case LType.String => return Seq.fill(CVEC_SIZE)(StrValue(scala.util.Random.alphanumeric.take(5).mkString))
            case LType.Boolean => {
                val seq = Seq(true, false) ++ Seq.fill(CVEC_SIZE - 2)(scala.util.Random.nextBoolean())
                seq.map(b => BoolValue(b))
            }
            case LType.List(of) => {
                return Seq.fill(CVEC_SIZE) {
                    val size = scala.util.Random.nextInt(3)
                    // generate a random number of cvecs to fill this list cvec
                    val els = (0 until size).map(_ => ListValue(generate_cvec(of)))
                    ListValue(els)
                }
            }
            case _ => throw new Exception("Unknown type in vector generation: " + t)
        }
    }

    // op is single letter character
    private def is_var[G](egraph: G, x: ENode)(using EGraphOps[G]): Boolean = {
      val xc = egraph.find(EClass(x))
      val xc_var = vars_analysis.getData(xc.id).get // data is Boolean
      
      return xc_var
    }

    // op is a number
    private def is_not_operation(x: ENode): Boolean = {
        val op = x.op.toString
        val operation = Op.fromString(op)
        operation match {
            case Op.UNKNOWN => return true // not an operation
            case _ => return false
        }
    }

    /**
      * Goal: check for contradicting cvecs
      *
      * @param data1
      * @param data2
      * @return merged cvec data or warning
      */
    def merge(data1: Data, data2: Data): Data = {
        // check if cvecs are the same
        val varsData = preMergeData(vars_analysis).asInstanceOf[(Boolean, Boolean)]

        if (varsData._1 && varsData._2) {
            // both are variables -> merge is ok and cvec is merged too (assuming types were checked in type_analysis)
            // make cvec of CVEC_SIZE taking a value from either data1 or data2, one of each at a time
            return (0 until CVEC_SIZE).map(i => if (i % 2 == 0) data1(i) else data2(i))
        }

        if (data1 != data2) {
            printWarning(s"Warning: Merging two different cvecs -> contradiction")
        }
        return data1
    }

    /**
      * Goal: Manages disequality analysis disunion.
      *
      * @param egraph graph
      * @param id class id
      */
    def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
        val cur_cvec = this.getData(id).get
        val diseq_set = disequality_analysis.getData(id).get
        // compare cur_cvec to all other cvecs. those that are different call disequality_analysis.disunion
        eclass_data.foreach{ case (other_id, other_cvec) =>
            // 3 conditions: not already in disequality set, not same id, different cvecs
            if (!(diseq_set.contains(other_id)) && other_id != id && other_cvec != cur_cvec) {
                disequality_analysis.disunion(id, other_id)
            }
        }
    }

    /**
      * Goal: expose disequality_analysis.is_consistent.
      * 
      * Note: [[is_consistent]] is a method to be called when we want to check the overall consistency of the egraph,
      * but doesn't really give much information. This exists in part because we cannot know when merging
      * disequality sets if we are merging a set that contains contradictions directly (because we don't know ids there).
      * 
      * @param egraph graph
      * @return consistency boolean
      */
    def is_consistent[G](egraph: G)(using EGraphOps[G]): Boolean = {
        return disequality_analysis.is_consistent(egraph)
    }
}
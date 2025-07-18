package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import propel.evaluator.egraph.mutable.simple.analysisExamples.TypeFoldAnalysis
import propel.evaluator.egraph.mutable.simple.{Op, Expr, Value, LType}
import propel.evaluator.egraph.mutable.simple.Expr.*
import propel.evaluator.egraph.mutable.simple.Value.*
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
    type Data = Seq[Value]
    val eclass_data = MutableMap()

    type GlobalData = Boolean // not relevant now
    var global_data = false

    val dependencies = List(type_analysis)

    private val CVEC_SIZE = 5

    // Helper debug function
    def printWarning(msg: String): Unit = {
        global_data = true
        println(s"WARNING: $msg")
    }

    override def operations(op: Op, ids: Option[Seq[EClass.Id]] = None): Function1[Seq[Data], Data] = {
        // Note: arity and typing are assumed to be correct (checked in TypeFoldAnalysis)
        assert(ids.isDefined, printWarning("CVecAnalysis operations called with undefined ids"))
        val children : Seq[type_analysis.Data] = ids.get.map(id => type_analysis.getData(id).get)
        // Use of Seq here is a workaround because I was lazy to do it properly before. A refactor is in order.
        op match {
            case Op.PLUS => (a : Seq[Seq[Value]]) => { val args = a(0); children match
                case Seq(LType.Number, LType.Number) => Seq(NumValue(getValueNum(args(0)) + getValueNum(args(1))))
                case Seq(LType.String, LType.String) => Seq(StrValue(getValueStr(args(0)) + getValueStr(args(1))))
                case Seq(LType.List(of), LType.List(of2)) => Seq(ListValue(elements = getElementsList(args(0)) ++ getElementsList(args(1))))
                case _ => printWarning(s"Don't know how to $op with args (${args.mkString(", ")})"); Seq()
            }
            case Op.MINUS => (a : Seq[Seq[Value]]) => { val args = a(0); children match
                case Seq(LType.Number, LType.Number) => Seq(NumValue(getValueNum(args(0)) - getValueNum(args(1))))
                case _ => printWarning(s"Don't know how to $op with args (${args.mkString(", ")})"); Seq()
            }
            case Op.MULT => (a : Seq[Seq[Value]]) => { val args = a(0); children match
                case Seq(LType.Number, LType.Number) => Seq(NumValue(getValueNum(args(0)) * getValueNum(args(1))))
                case _ => printWarning(s"Don't know how to $op with args (${args.mkString(", ")})"); Seq()
            }
            case Op.UNKNOWN => args => { printWarning(s"Unknown operator: $op with args (${args.mkString(", ")})"); Seq()}
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

        if(is_var(x)) {
            // add variable x.op to the list of known variables
            if (xc_type == LType.Function) {
                throw new Exception("Function type not supported for characteristic vector generation")
            }
            val cvec = generate_cvec(xc_type)
            eclass_data.update(xc.id, cvec)
            return cvec
        }

        // TODO: improve this check (should be called "is_value" as in, something that is not a name of something)
        if(is_const(x)) {
            eclass_data.update(xc.id, Seq.fill(CVEC_SIZE)(StrValue(x.op.toString))) // e.g. "2" -> Seq(2, 2, 2, 2, 2, 2, 2, 2, 2, 2) because it always means 2
            return Seq.fill(CVEC_SIZE)(StrValue(x.op.toString))
        }

        // lastly: not var and not const -> expecting an operation between classes (i.e. a function application)

        // build cvec one position at a time by applying the operation to each value from the referenced classes
        val children = x.refs.map(cc => egraph.find(cc)) // canonicalized children
        val children_values = children.map(c => eclass_data.getOrElse(c.id, throw new Exception("No data found for child during cvec generation: " + c.id)))
        var cvec = Seq.empty[Value]
        for (i <- 0 until CVEC_SIZE) {
            // Note: typing is assumed to be correct (checked in TypeFoldAnalysis)
            val args = children_values.map(_.apply(i)) // select i-th of each
            val f = operations(
                Op.fromString(x.op.toString),
                Some(children.map(_.id))
            )
            // assert(f != Unit, printWarning(s"WARNING: Unknown function during cvec generation: " + x.op))

            val res = f(Seq(args)) // res should be an Value
            // assert(res(0) != Unit, printWarning(s"Function returned null during cvec generation: " + x.op))
            cvec = cvec :+ res(0)
        }
        eclass_data.update(xc.id, cvec)
        return cvec
    }

    // TODO: pass CVEC_SIZE as a parameter
    private def generate_cvec(t: LType): Seq[Value] = {
        t match {
            case LType.Number => return Seq.fill(CVEC_SIZE)(NumValue(scala.util.Random.nextInt(20)))
            case LType.String => return Seq.fill(CVEC_SIZE)(StrValue(scala.util.Random.alphanumeric.take(5).mkString))
            case LType.Boolean => {
                val seq = Seq(true, false) ++ Seq.fill(CVEC_SIZE - 2)(scala.util.Random.nextBoolean())
                seq.map(b => BoolValue(b))
            }
            case LType.List(of) => {
                return Seq.fill(CVEC_SIZE) {
                    val size = scala.util.Random.nextInt(3) + 1
                    val els = of match {
                        case LType.Number => Seq.fill(size)(NumValue(scala.util.Random.nextInt(20)))
                        case LType.String => Seq.fill(size)(StrValue(scala.util.Random.alphanumeric.take(5).mkString))
                        case LType.Boolean => Seq.fill(size)(BoolValue(scala.util.Random.nextBoolean()))
                        case LType.List(_) => Seq.fill(CVEC_SIZE)(ListValue(generate_cvec(of))) // TODO: this does not respect size. i should fix this because it is a technically a mistake
                        case _ => throw new Exception("Unsupported type in list generation: " + of)
                    }
                    ListValue(els)
                }
            }
            case _ => throw new Exception("Unknown type in vector generation: " + t)
        }
    }

    // op is single letter character
    private def is_var(x: ENode): Boolean = {
        // TODO: this can be replaced with a variable map / another analysis for this purpose
        val op = x.op.toString
        return (op.length == 1 && op.head.isLetter)
    }

    // op is a number
    private def is_const(x: ENode): Boolean = {
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
      * @return data1
      */
    def merge(data1: Data, data2: Data): Data = {
        // check if cvecs are the same
        if (data1 != data2) {
            printWarning(s"Warning: Merging two different cvecs -> contradiction")
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
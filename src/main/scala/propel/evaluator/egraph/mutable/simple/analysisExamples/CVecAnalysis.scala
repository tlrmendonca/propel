package propel.evaluator.egraph.mutable.simple.analysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import propel.evaluator.egraph.mutable.simple.analysisExamples.TypeAnalysis
import propel.evaluator.egraph.mutable.simple.Value
import propel.evaluator.egraph.mutable.simple.Value.*
import propel.evaluator.egraph.mutable.simple.Type
import propel.evaluator.egraph.mutable.simple.Type.*
import propel.evaluator.egraph.mutable.simple.Op
import propel.evaluator.egraph.mutable.simple.Op.*
import propel.evaluator.egraph.mutable.simple.ConstructorName
import propel.evaluator.egraph.mutable.simple.ConstructorName.*
import scala.annotation.varargs
import propel.dsl.impl.Checked.check

/**
  * [[Characteristic Vectors]]
  * Goal: Associate with each class a list of values that depend on its connection to other classes.
  * E.g.: x is assigned the randomly generated list [0, 3, 25, 100], then 2x is assigned the list [0, 6, 50, 200].
  * This allows a quick way to prove inequalities and efficient pruning of the lemma candidates space.
  */
class CVecAnalysis(
    type_analysis: TypeAnalysis,
    vars_analysis: SimpleVarsAnalysis,
    disequality_analysis: DisequalityAnalysis,
    expr_extractor_analysis: ExprExtractorAnalysis
) extends Analysis {
    /**
      * [[Data]] set as [[Seq<EClass.Id>]] to refer to other classes.
      */
    type Data = Seq[Value] // Value is important because I want my CVECs to not be "simplifiable" anymore
    val eclass_data = MutableMap()

    type GlobalData = Boolean // not relevant now
    var global_data = false

    val dependencies = List(type_analysis, vars_analysis, disequality_analysis, expr_extractor_analysis)

    private val CVEC_SIZE = 5

    // Helper debug function
    def printWarning(msg: String): Unit = {
        global_data = true
        println(s"WARNING: $msg")
    }

    override def operations(op: Op, children_ids: Option[Seq[EClass.Id]] = None): Option[Function1[Seq[Data], Data]] = {
        // Note: arity and typing are assumed to be correct (checked in TypeAnalysis)
        assert(children_ids.isDefined, printWarning("CVecAnalysis operations called with undefined children_ids"))
        
        op match {
            case PLUS => Some(args => {
                val vals1 = args(0); val vals2 = args(1)
                (vals1 zip vals2).map { (v1, v2) =>
                    def fromNat(v: Value): Int = v match
                        case ValueConstructor(ConstructorName.Zero, _) => 0
                        case ValueConstructor(ConstructorName.Succ, Seq(sub)) => 1 + fromNat(sub)
                        case ValueConstructor(ConstructorName.Pred, Seq(sub)) => -1 + fromNat(sub)
                        case _ => 0
                    def toNat(i: Int): Value = if (i == 0) ValueConstructor(ConstructorName.Zero, Seq()) else if (i > 0) ValueConstructor(ConstructorName.Succ, Seq(toNat(i - 1))) else ValueConstructor(ConstructorName.Pred, Seq(toNat(i + 1)))
                    toNat(fromNat(v1) + fromNat(v2))
                }
            })
            case MINUS | MULT | DIV | POW2 | SQRT | MAX | MIN | MOD => Some(args => {
                val vals1 = args(0)
                if (op == POW2 || op == SQRT) {
                  vals1.map { v1 =>
                    def fromNat(v: Value): Int = v match
                        case ValueConstructor(ConstructorName.Zero, _) => 0
                        case ValueConstructor(ConstructorName.Succ, Seq(sub)) => 1 + fromNat(sub)
                        case ValueConstructor(ConstructorName.Pred, Seq(sub)) => -1 + fromNat(sub)
                        case _ => 0
                    def toNat(i: Int): Value = if (i == 0) ValueConstructor(ConstructorName.Zero, Seq()) else if (i > 0) ValueConstructor(ConstructorName.Succ, Seq(toNat(i - 1))) else ValueConstructor(ConstructorName.Pred, Seq(toNat(i + 1)))
                    val val1 = fromNat(v1)
                    val res = op match
                        case POW2 => val1 * val1
                        case SQRT => Math.sqrt(val1).toInt
                        case _ => 0
                    toNat(res)
                  }
                } else {
                  val vals2 = args(1)
                  (vals1 zip vals2).map { (v1, v2) =>
                    def fromNat(v: Value): Int = v match
                        case ValueConstructor(ConstructorName.Zero, _) => 0
                        case ValueConstructor(ConstructorName.Succ, Seq(sub)) => 1 + fromNat(sub)
                        case ValueConstructor(ConstructorName.Pred, Seq(sub)) => -1 + fromNat(sub)
                        case _ => 0
                    def toNat(i: Int): Value = if (i == 0) ValueConstructor(ConstructorName.Zero, Seq()) else if (i > 0) ValueConstructor(ConstructorName.Succ, Seq(toNat(i - 1))) else ValueConstructor(ConstructorName.Pred, Seq(toNat(i + 1)))
                    val val1 = fromNat(v1); val val2 = fromNat(v2)
                    val res = op match
                        case MINUS => val1 - val2
                        case MULT => val1 * val2
                        case DIV => if (val2 == 0) 0 else val1 / val2
                        case MAX => Math.max(val1, val2)
                        case MIN => Math.min(val1, val2)
                        case MOD => if (val2 == 0) 0 else val1 % val2
                        case _ => 0
                    toNat(res)
                  }
                }
            })
            case ISZERO => Some(args => {
                args(0).map { v =>
                    v match
                        case ValueConstructor(ConstructorName.Zero, _) => ValueConstructor(ConstructorName.True, Seq())
                        case _ => ValueConstructor(ConstructorName.False, Seq())
                }
            })
            case LESSTHAN | GREATERTHAN | EQUALS => Some(args => {
                val vals1 = args(0); val vals2 = args(1)
                (vals1 zip vals2).map { (v1, v2) =>
                    def fromNat(v: Value): Int = v match
                        case ValueConstructor(ConstructorName.Zero, _) => 0
                        case ValueConstructor(ConstructorName.Succ, Seq(sub)) => 1 + fromNat(sub)
                        case ValueConstructor(ConstructorName.Pred, Seq(sub)) => -1 + fromNat(sub)
                        case _ => 0
                    val res = op match
                        case LESSTHAN => fromNat(v1) < fromNat(v2)
                        case GREATERTHAN => fromNat(v1) > fromNat(v2)
                        case EQUALS => fromNat(v1) == fromNat(v2)
                        case _ => false
                    if (res) ValueConstructor(ConstructorName.True, Seq()) else ValueConstructor(ConstructorName.False, Seq())
                }
            })
            case AND | OR => Some(args => {
                val vals1 = args(0); val vals2 = args(1)
                (vals1 zip vals2).map { (v1, v2) =>
                    val b1 = v1 match { case ValueConstructor(ConstructorName.True, _) => true; case _ => false }
                    val b2 = v2 match { case ValueConstructor(ConstructorName.True, _) => true; case _ => false }
                    val res = op match
                        case AND => b1 && b2
                        case OR => b1 || b2
                        case _ => false
                    if (res) ValueConstructor(ConstructorName.True, Seq()) else ValueConstructor(ConstructorName.False, Seq())
                }
            })
            case NOT => Some(args => {
                args(0).map { v =>
                    val b = v match { case ValueConstructor(ConstructorName.True, _) => true; case _ => false }
                    if (!b) ValueConstructor(ConstructorName.True, Seq()) else ValueConstructor(ConstructorName.False, Seq())
                }
            })
            case APPEND => Some(args => {
                val lists1 = args(0); val lists2 = args(1)
                (lists1 zip lists2).map { (l1, l2) => appendLists(l1, l2) }
            })
            case REVERSE => Some(args => {
                args(0).map(reverseList)
            })
            case LENGTH => Some(args => {
                args(0).map { l =>
                    def len(v: Value): Int = v match
                        case ValueConstructor(ConstructorName.Nil, _) => 0
                        case ValueConstructor(ConstructorName.Cons, Seq(_, tail)) => 1 + len(tail)
                        case _ => 0
                    def toNat(i: Int): Value = if (i == 0) ValueConstructor(ConstructorName.Zero, Seq()) else ValueConstructor(ConstructorName.Succ, Seq(toNat(i - 1)))
                    toNat(len(l))
                }
            })
            case UNKNOWN => Some(args => {
                // Placeholder, handled by make() for UNKNOWN names
                Seq.fill(CVEC_SIZE)(ValueConstructor(ConstructorName.Zero, Seq()))
            })
        }
    }

    private def appendLists(a: Value, b: Value): Value = a match {
        case ValueConstructor(ConstructorName.Nil, _) => b
        case ValueConstructor(ConstructorName.Cons, Seq(head, tail)) =>
            ValueConstructor(ConstructorName.Cons, Seq(head, appendLists(tail, b)))
        case _ => b
    }

    private def reverseList(v: Value): Value = v match {
        case ValueConstructor(ConstructorName.Nil, _) => ValueConstructor(ConstructorName.Nil, Seq())
        case ValueConstructor(ConstructorName.Cons, Seq(head, tail)) =>
            appendLists(reverseList(tail), ValueConstructor(ConstructorName.Cons, Seq(head, ValueConstructor(ConstructorName.Nil, Seq()))))
        case _ => ValueConstructor(ConstructorName.Nil, Seq())
    }

    private def check_type(xc_type: Type, op: String) : Value = {
      xc_type match {
        case Type.Nat => {
            try {
                val n = op.toInt
                def toNat(i: Int): Value = if (i == 0) ValueConstructor(ConstructorName.Zero, Seq()) else if (i > 0) ValueConstructor(ConstructorName.Succ, Seq(toNat(i - 1))) else ValueConstructor(ConstructorName.Pred, Seq(toNat(i + 1)))
                toNat(n)
            } catch {
                case _: Exception => ValueConstructor(ConstructorName.Zero, Seq())
            }
        }
        case Type.Boolean => {
            if (op == "true") ValueConstructor(ConstructorName.True, Seq())
            else ValueConstructor(ConstructorName.False, Seq())
        }
        case Type.TList => ValueConstructor(ConstructorName.Nil, Seq())
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
            if (xc_type == Type.Function) {
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
            val value_of : Value = check_type(xc_type, x.op.toString)

            val res = Seq.fill(CVEC_SIZE)(value_of)
            eclass_data.update(xc.id, res) // e.g. "2" -> Seq(2, 2, 2, 2, 2, 2, 2, 2, 2, 2) because it always means 2
            return res
        }

        // lastly: not var and not const -> expecting an operation between classes (i.e. a function application)

        // build cvec one position at a time by applying the operation to each value from the referenced classes
        val children = x.refs.map(cc => egraph.find(cc)) // canonicalized children
        val children_values = children.map(c => eclass_data.getOrElse(c.id, throw new Exception("No data found for child during cvec generation: " + c.id)))
        
        val op = Op.fromString(x.op.toString)
        val fo = operations(op, Some(children.map(_.id)))
        assert(fo.isDefined, printWarning(s"WARNING: Undefined/unknown function during cvec generation: " + x.op))
        val f = fo.get
        val res = f(children_values)

        eclass_data.update(xc.id, res)
        return res
    }

    // TODO: pass CVEC_SIZE as a parameter
    private def generate_cvec(t: Type): Seq[Value] = {
        t match {
            case Type.Nat => Seq.fill(CVEC_SIZE)({
                val n = util.Random.between(-50, 51)
                def toNat(i: Int): Value = if (i == 0) ValueConstructor(ConstructorName.Zero, Seq()) else if (i > 0) ValueConstructor(ConstructorName.Succ, Seq(toNat(i - 1))) else ValueConstructor(ConstructorName.Pred, Seq(toNat(i + 1)))
                toNat(n)
            })
            case Type.Boolean => {
                Seq.fill(CVEC_SIZE)(scala.util.Random.nextBoolean())
                  .map(b => if (b) ValueConstructor(ConstructorName.True, Seq()) else ValueConstructor(ConstructorName.False, Seq()))
            }
            case Type.TList => Seq.fill(CVEC_SIZE)({
                val len = util.Random.between(0, 4)
                def randomNat(): Value = {
                    val n = util.Random.between(0, 4)
                    def toNat(i: Int): Value = if (i == 0) ValueConstructor(ConstructorName.Zero, Seq()) else ValueConstructor(ConstructorName.Succ, Seq(toNat(i - 1)))
                    toNat(n)
                }
                def makeList(remaining: Int): Value =
                    if (remaining == 0) ValueConstructor(ConstructorName.Nil, Seq())
                    else ValueConstructor(ConstructorName.Cons, Seq(randomNat(), makeList(remaining - 1)))
                makeList(len)
            })
            case _ => throw new Exception("Unknown type in vector generation: " + t)
        }
    }

    private def is_not_operation(x: ENode): Boolean = {
        val op = x.op.toString
        if (op == "true" || op == "false") return true
        // Check if it's a number string
        if (op.forall(_.isDigit)) return true
        
        val operation = Op.fromString(op)
        operation match {
            case Op.UNKNOWN => return true // not an operation
            case _ => return false
        }
    }

    // op is single letter character
    private def is_var[G](egraph: G, x: ENode)(using EGraphOps[G]): Boolean = {
      val xc = egraph.find(EClass(x))
      val xc_data = vars_analysis.getData(xc.id).get // data is Option[Type]
      
      return xc_data.isDefined
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
        val varsData = preMergeData(vars_analysis).asInstanceOf[(Option[Type], Option[Type])]

        if (varsData._1.isDefined && varsData._2.isDefined) {
            // both are variables -> merge is ok and cvec is merged too (assuming types were checked in type_analysis)
            // make cvec of CVEC_SIZE taking a value from either data1 or data2, one of each at a time
            return (0 until CVEC_SIZE).map(i => if (i % 2 == 0) data1(i) else data2(i))
        }

        if (data1 != data2) {
            printWarning(s"Warning: Merging two different cvecs -> warning of a potential contradiction if this wasn't caused on purpose")
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
        // TODO: optimize this loop to avoid checking all eclasses, perhaps by keeping a set of already compared ids or perhaps a best effort approach
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

    /**
      * Goal: Return a list of pairs of EClass ids that are potentially equal (all but the ones we know are disequal).
      * Run them thru the extractor to get expressions for clarity purposes.
      */
    def conjecture_lemmas() : Set[(String, String)] = {
        // TODO: group by cvec and propose the combinations inside each group
        val lemmas = MutableSet[(String, String)]()
        // for each pair of eclasses, if they are not disequal, add to lemmas
        val eclass_ids = eclass_data.keys.toSeq
        for (i <- 0 until eclass_ids.length) {
            for (j <- i + 1 until eclass_ids.length) {
                val id1 = eclass_ids(i)
                val id2 = eclass_ids(j)
                if (!disequality_analysis.getData(id1).get.contains(id2)) {
                    // not disequal -> add to lemmas
                    val expr1 = expr_extractor_analysis.getData(id1).get._1
                    val expr2 = expr_extractor_analysis.getData(id2).get._1
                    lemmas.add((expr1, expr2))
                }
            }
        }
        return lemmas.toSet
    }
}
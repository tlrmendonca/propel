package propel.evaluator.egraph.mutable.simple.AnalysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet}

object ConstantFoldAnalysis {
    // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisExamples.testEGraph"
    @main def testEGraph(): Unit =
        import EGraph.EGraphOps
        val empty_analysis = new Analysis {
            type Data = Int
            val eclass_data = MutableMap()
            def make[A <: Analysis, G[_ <: A]](g: G[A], x: ENode)(using EGraphOps[A, G]): Data = {
            val xc = g.find(EClass(x))
            eclass_data.update(xc.id, 0)
            println(s"make on ($xc.id) and set data to (${eclass_data(xc.id)})")
            0
            }
            def merge(data1: Data, data2: Data): Data = {
            println(s"merge($data1, $data2)")
            1
            }
            def modify[A <: Analysis, G[_ <: A]](egraph: G[A], id: EClass.Id)(using EGraphOps[A, G]): Unit = {
            println(s"modify($id)")
            }
        }

        val egraph = EGraph(empty_analysis)

        /** Define the elements of your egraphs. */
        val constantsENodes @ Seq(an,bn,cn,dn,en,fn,gn,hn,in) = Seq(
            ENode(Operator("a")),
            ENode(Operator("b")),
            ENode(Operator("c")),
            ENode(Operator("d")),
            ENode(Operator("e")),
            ENode(Operator("f")),
            ENode(Operator("g")),
            ENode(Operator("h")),
            ENode(Operator("i")),
        )
        val constantsEClasses @ Seq(a, b, c, d, e, f, g, h, i) =
            constantsENodes.map(egraph.add)

        val operationsENodes @ Seq(fan, fbn) = Seq(
            ENode(Operator("f"), Seq(a)),
            ENode(Operator("f"), Seq(b)),
        )
        val operationsEClasses @ Seq(fa, fb) = operationsENodes.map(egraph.add)

        /** Encode equalities between the elements of your egraphs. */
        // Equality Set: {a=b; c=d; e=f; c=b}
        egraph.union(a, b)
        egraph.union(c, d)
        egraph.union(e, f)
        egraph.union(c, b)

        /** Rebuild egraph to restore congruence invariance. */
        // Congruence Invariance: Vf Vx Vy. x=y -> f(x)=f(y)
        // In this case, a=b -> f(a)=f(b)
        egraph.rebuild()

        println("\nCANONICALIZATION:")
        println(s"canonicalize($fan): ${egraph.canonicalize(fan)}")
        println(s"canonicalize($fbn): ${egraph.canonicalize(fbn)}")

        println("\nCANONICALS:")
        println((constantsEClasses ++ operationsEClasses).map(e => s"$e -> ${egraph.find(e)}").mkString("; "))

        println("\nECLASSES")
        println(s"eclasses: ${egraph.eclasses.map(_ -> _.mkString("{",";","}"))}")
        println(s"# eclasses: ${egraph.eclasses.size}")
        println(s"# enodes: ${egraph.eclasses.foldLeft(0)(_ + _._2.size)}")

        println("\nEQUALITY:")
        println(s"a = b: ${egraph.equal(a, b)}")
        println(s"f(a) = f(b): ${egraph.equal(fa, fb)}")
        // ^ Note that you never explicitly add the equality f(a) = f(b) to the egraph,
        //   but it is inferred from the equality a = b due to congruence invariance.
}
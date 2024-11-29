package propel.evaluator.egraph.mutable.simple.AnalysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet}

object ConstantFoldAnalysis {

    def prettyPrintEClasses(eclasses: Map[EClass, Set[ENode]]): String = {
      eclasses.map { case (eclass, enodes) =>
        s"$eclass -> ${enodes.mkString(",")}"
      }.mkString("; ")
    }
    // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisExamples.testConstantFold"
    @main def testConstantFold(): Unit =
        import EGraph.EGraphOps

        /**
          * [[Constant Folding]]
          * Level: Simple
          * Goal: Assert that all enodes deemed equal have the same constant value
          */
        val constant_fold_analysis = new Analysis {

            /**
              * [[Data]] set as [[String]] to use [[ENode]]'s [[Operator]] as a constant value.
              */
            type Data = String
            val eclass_data = MutableMap()

            /**
              * Goal: Calculate the constant value if dependent on children.
              * @note Let us assume a node can only have one children.
              *
              * @param g graph
              * @param x node
              * @return constant value of said node
              */
            def make[A <: Analysis, G[_ <: A]](egraph: G[A], x: ENode)(using EGraphOps[A, G]): Data = {
              // define data
              var cdata = ""
              if !x.refs.isEmpty then
                cdata = getData(x.refs.head.id).get
              var data = x.op.toString + cdata

              // exception - if op is y, then its x (simulating x is y conceptually)
              // this is a *shortcut* to writting a decent example
              if data == "y" then data = "x"

              val xc = egraph.find(EClass(x))
              eclass_data.update(xc.id, data)

              println(s"Make: class ${xc} added and data set to ${eclass_data(xc.id)}")
              return data
            }

            /**
              * Goal: Assert two classes have the same constant value.
              *
              * @param data1 data
              * @param data2 data
              * @return
              */
            def merge(data1: Data, data2: Data): Data = {
              if (data1 == data2) 
                println(s"Merge: $data1 and $data2 are equal")
                data1
              else throw new Exception(s"Inconsistent data! Cannot merge ($data1) and ($data2)")  
            }

            /**
              * Goal: "Concretize" the constant value into a node, i.e. add a node to the class.
              *
              * @param egraph graph
              * @param id class id
              */
            def modify[A <: Analysis, G[_ <: A]](egraph: G[A], id: EClass.Id)(using EGraphOps[A, G]): Unit = {
              val data = getData(id).get
              val c = egraph.getEClassFromId(id)
              val n = ENode(Operator(data))
              // println("Wanted to add node: " + n)
              val nc = egraph.add(n)
              egraph.union(c, nc)
            }
        }

        /**
          * Goals: 
          * 1. Union two equal values
          * 2. Assert correct data creation and modify working (on node with a child)
          * 3. Union two equivalent nodes (different children)
          * 4. Assert two different nodes crash
          * 5. Assert two equal value nodes with different children crash 
          */
        
        val egraph = EGraph(constant_fold_analysis)

        // Goal 1
        println("\n*Goal 1* - Union two equal values")
        val equalENodes @ Seq(xn,yn) = Seq(
          ENode(Operator("x")),
          ENode(Operator("y")),
        )
        val equalEClasses @ Seq(x, y) =
          equalENodes.map(egraph.add)
        
        egraph.union(x, y)
        // ^ verify that the union is successful

        // Goal 2
        println("\n*Goal 2* - Assert correct data creation and modify working (on node with a child)")
        val constantsENodes @ Seq(an,bn) = Seq(
          ENode(Operator("a")),
          ENode(Operator("b")),
        )
        val constantsEClasses @ Seq(a, b) =
          constantsENodes.map(egraph.add)

        val en = ENode(Operator("e"), Seq(a))
        val constantsEClasses2 @ Seq(e) = Seq(en).map(egraph.add)
        // ^ verfiy make print produces "ea"
        
        println("CANONICALS:")
        println((constantsEClasses ++ constantsEClasses2).map(e => s"$e -> ${egraph.find(e)}").mkString("; "))
        println(prettyPrintEClasses(egraph.eclasses))
        // ^ verify that modify adds the node "ea" instead of "e(a)"

        // Goal 3
        println("\n*Goal 3* - Union two equivalent nodes (different children)")
        val equivalentENodes1 @ Seq(hn, ghn) = Seq(
          ENode(Operator("h")),
          ENode(Operator("gh")),
        )
        val equivalentEClasses1 @ Seq(h, gh) =
          equivalentENodes1.map(egraph.add)

        val equivalentENodes2 @ Seq(fgn, fn) = Seq(
          ENode(Operator("fg"), Seq(h)),
          ENode(Operator("f"), Seq(gh)),
        )
        val equivalentEClasses2 @ Seq(fg, f) =
          equivalentENodes2.map(egraph.add)
        // ^ created nodes fg(h) and f(gh) which should both have data "fgh"

        egraph.union(fg, f)
        // ^ verify that the union is successful

        // Goal 4
        println("\n*Goal 4* - Assert two different nodes crash")
        val differentENodes @ Seq(cn,dn) = Seq(
          ENode(Operator("c")),
          ENode(Operator("d")),
        )
        val differentEClasses @ Seq(c, d) =
          differentENodes.map(egraph.add)

        val exception = try {
          egraph.union(c, d)
          None
        } catch {
          case e: Exception => Some(e)
        }
        assert(exception.isDefined)
        println(s"Exception reached: ${exception.get.getMessage}")
        // ^ verify that the union is unsuccessful

        // Goal 5
        println("\n*Goal 5* - Assert two equal value nodes with different children crash")
        val differentChildrenENodes @ Seq(icn, idn) = Seq(
          ENode(Operator("i"), Seq(c)),
          ENode(Operator("i"), Seq(d)),
        )
        val differentChildrenEClasses @ Seq(ic, id) =
          differentChildrenENodes.map(egraph.add)
        
        println(prettyPrintEClasses(egraph.eclasses))
        val exception2 = try {
          egraph.union(ic, id)
          None
        } catch {
          case e: Exception => Some(e)
        }
        assert(exception2.isDefined)
        println(s"Exception reached: ${exception2.get.getMessage}")
        // ^ verify that the union is unsuccessful
}
package propel.evaluator.egraph.mutable.simple.AnalysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet}

object CountingNodesAnalysis {

    def prettyPrintEClasses(eclasses: Map[EClass, Set[ENode]]): String = {
      eclasses.toSeq.sortBy(_._1.toString).map { case (eclass, enodes) =>
        s"$eclass -> ${enodes.mkString(",")}"
      }.mkString("; ")
    }
    
    def prettyPrintData[D](data: Map[EClass.Id, D]): String = {
      data.toSeq.sortBy(_._1.toString).map { case (id, data) =>
        val str = id.toString.stripPrefix("Symbol(").stripSuffix(")")
        s"$str -> $data"
      }.mkString("; ")
    }

    // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisExamples.testCountingNodes"
    @main def testCountingNodes(): Unit =
        import EGraph.EGraphOps

        /**
          * [[Counting Nodes in Classes]]
          * Level: Easy
          * Goal: Keep track of what nodes exist in each class
          * 
          * [[Tracking Nodes in Class]]
          * Level: Easy
          * Goal: Keep track of what nodes exist in each class and how many nodes are in each class
          */
        val counting_nodes_analysis = new Analysis {

            /**
              * [[Data]] set as [[String]] to use [[ENode]]'s [[Operator]] as a constant value.
              */
            type Data = (Int, Seq[ENode])
            val eclass_data = MutableMap()

            /**
              * Goal: Set data (number of nodes in class) to 1.
              * 
              * Goal2: Set data to (1, node).
              *
              * @param g graph
              * @param x node
              * @return Tuple (1, node)
              */
            def make[A <: Analysis, G[_ <: A]](egraph: G[A], x: ENode)(using EGraphOps[A, G]): Data = {
              val xc = egraph.find(EClass(x))
              eclass_data.update(xc.id, (1, Seq(x)))

              return (1, Seq(x))
            }

            /**
              * Goal: Add data of two classes.
              * 
              * Goal2: Merge data of two classes by adding the number of nodes and concatenating the sequences.
              * @note Sequences will never have nodes in common, since that would imply duplication of nodes.
              *
              * @param data1 data
              * @param data2 data
              * @return Tuple (data1._1 + data2._1, data1._2 ++ data2._2)
              */
            def merge(data1: Data, data2: Data): Data = {
              println(s"Merging $data1 and $data2")
              return (data1._1 + data2._1, data1._2 ++ data2._2)
            }

            /**
              * Goal: Empty
              * 
              * Goal2: Empty
              * @note This function is not necessary, but could be used for checking invariants if necessary.
              *
              * @param egraph graph
              * @param id class id
              */
            def modify[A <: Analysis, G[_ <: A]](egraph: G[A], id: EClass.Id)(using EGraphOps[A, G]): Unit = {
              return
            }
        }

        /**
          * Goals: 
          * 1. Assert data is being created
          * 2. Merge of two single-node classes is 2
          * 3. Merge of two multi-node classes is the sum of the nodes
          */
        
        val egraph = EGraph(counting_nodes_analysis)

        // Goal 1
        println("\n*Goal 1* - Assert correct data creation")
        val constantENodes @ Seq(an, bn, cn, dn, en) = Seq(
          ENode(Operator("a")),
          ENode(Operator("b")),
          ENode(Operator("c")),
          ENode(Operator("d")),
          ENode(Operator("e")),
        )
        val constantEClasses @ Seq(a, b, c, d, e) =
          constantENodes.map(egraph.add)
        
        println(prettyPrintEClasses(egraph.eclasses))
        println(prettyPrintData(counting_nodes_analysis.eclass_data.toMap))
        // ^ verify everything is set to 1

        // Goal 2
        println("\n*Goal 2* - Merge of two single-node classes is 2")
        
        egraph.union(a, b)
        egraph.rebuild()

        println(prettyPrintEClasses(egraph.eclasses))
        println(prettyPrintData(counting_nodes_analysis.eclass_data.toMap))
        // ^ verify class with nodes a,b has data 2

        // Goal 3
        println("\n*Goal 3* - Merge of two multi-node classes is the sum of the nodes")
        egraph.union(c, d)
        egraph.union(c, e)
        egraph.rebuild()
        
        println(prettyPrintEClasses(egraph.eclasses))
        println(prettyPrintData(counting_nodes_analysis.eclass_data.toMap))
        // ^ verify class with nodes c,d,e has data 3

        egraph.union(a, c)
        egraph.rebuild()

        println(prettyPrintEClasses(egraph.eclasses))
        println(prettyPrintData(counting_nodes_analysis.eclass_data.toMap))
        // ^ verify class with nodes a,b,c,d,e has data 5
}
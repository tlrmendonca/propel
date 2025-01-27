package propel.evaluator.egraph.mutable.simple.AnalysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

object DisequalityAnalysis {

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

  // Tag to restrict usability
  trait DisequalityTag

  // Type alias for simplification below
  type DisequalityAnalysisT = Analysis & DisequalityTag {
    type Data = Set[EClass.Id]
    val eclass_data: MutableMap[EClass.Id, Set[EClass.Id]]
  }

  /**
    * [[Extension]], using the type of the analysis, featuring [[user-callable]] functions to access the analysis.
    */
  object DisequalityAnalysisExtensions {
    // Uncomment in case EGraph is used inside extension
    import EGraph._
    
    extension (analysis: DisequalityAnalysisT) {
      /**
        * Goal: Add a disunion between two classes.
        * 
        * @param id1 first class id
        * @param id2 second class id
        */
      def disunion(id1: EClass.Id, id2: EClass.Id): Unit =
        // add id2 to first class
        val data1 = analysis.eclass_data.getOrElse(id1, Set.empty[EClass.Id])
        val newData1 = data1 + id2
        analysis.eclass_data.update(id1, newData1)

        // add id1 to second class
        val data2 = analysis.eclass_data.getOrElse(id2, Set.empty[EClass.Id])
        val newData2 = data2 + id1
        analysis.eclass_data.update(id2, newData2)
      
      /**
       * Goal: Check if the analysis is consistent, i.e, no class forbids itself.
       */
      def is_consistent(egraph: EGraph): Boolean =
        egraph.eclasses.forall(c =>
          val ccid = egraph.find(c._1).id
          analysis.getData(c._1.id).forall(fid =>
            val fc = egraph.getEClassFromId(fid)
            egraph.find(fc).id != ccid
          )
        )

        // possibly more efficient version
        // analysis.eclass_data.forall((cid, data) =>
        //   data.forall(id =>
        //     val fc = egraph.getEClassFromId(id)
        //     egraph.find(fc).id != cid
        //   )
        // )
    }
  }

  // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisExamples.testDisequality"
  @main def testDisequality(): Unit =
    import EGraph.EGraphOps

    /**
      * [[Disequalities between Classes]]
      * Level: Hard
      * Goal: Keep track of disequality relations.
      */
    val disequality_analysis = new Analysis with DisequalityTag {
      /**
        * [[Data]] set as [[Seq<EClass.Id>]] to refer to other classes.
        */
      type Data = Set[EClass.Id]
      val eclass_data = MutableMap()

      /**
        * Goal: Set sequence to empty.
        * 
        * @param g graph
        * @param x node
        * @return Seq[EClass.Id]
        */
      def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
        val xc = egraph.find(EClass(x))
        val emptySet: Set[EClass.Id] = Set.empty[EClass.Id]
        eclass_data.update(xc.id, emptySet)

        return emptySet
      }

      /**
        * Goal: Concatenate sequences.
        * 
        * @param data1 Seq of EClass.Id
        * @param data2 Seq of EClass.Id
        * @return Seq[EClass.Id] as the concatenation of data1 and data2
        */
      def merge(data1: Data, data2: Data): Data = {
        val mergedSet = data1 ++ data2
        return mergedSet
      }

      /**
        * Goal: Empty
        * 
        * @note This function could be used ot check for contradictions, but that would be expected to be
        * computationally intensive, therefore it will be implemented as a user-callable instead, check 
        * DisequalityAnalysisExtensions.is_consistent for more information.
        * 
        * @param egraph graph
        * @param id class id
        */
      def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
        return
      }
    }

    import DisequalityAnalysisExtensions.*

    /**
      * Goals: 
      * 1. Add disunions and pass the consistency test
      * 2. Add merge that breaks consistency and assert failure in the consistency test
      */
    
    val egraph = EGraph()
    egraph.addAnalysis(disequality_analysis)

    // Goal 1
    println("\n*Goal 1* - Assert correct data creation and consistency")
    val constantENodes @ Seq(an, bn, cn) = Seq(
      ENode(Operator("a")),
      ENode(Operator("b")),
      ENode(Operator("c")),
    )
    val constantEClasses @ Seq(a, b, c) =
      constantENodes.map(egraph.add)

    disequality_analysis.disunion(a.id, b.id)

    egraph.union(b, c)
    egraph.rebuild()

    // check consistency
    println(s"Consistency check: ${disequality_analysis.is_consistent()}")
    
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(disequality_analysis.eclass_data.toMap))

    // Goal 2
    println("\n*Goal 2* - Assert inconsistency")

    egraph.union(a, b) // illegal union
    egraph.rebuild()

    // check consistency
    println(s"Consistency check: ${disequality_analysis.is_consistent()}")

    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(disequality_analysis.eclass_data.toMap))
    // ^ note that a and b are both equal and disequal to each other
}
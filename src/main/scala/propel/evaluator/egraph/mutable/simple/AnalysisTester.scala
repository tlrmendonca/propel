package propel.evaluator.egraph.mutable.simple

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.simple.analysisExamples.*

object AnalysisTester {

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

  // Test CountingNodesAnalysis
  def testCountingNodes(): Unit =
    import EGraph.EGraphOps

    val counting_nodes_analysis = new CountingNodesAnalysis()
    
    /**
      * Goals: 
      * 1. Assert data is being created
      * 2. Merge of two single-node classes is 2
      * 3. Merge of two multi-node classes is the sum of the nodes
      */
    
    val egraph = EGraph()
    egraph.addAnalysis(counting_nodes_analysis)

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

  // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisTester"
  def main(args: Array[String]): Unit = {
    println("Starting AnalysisTester...")
    testCountingNodes()
    println("AnalysisTester completed.")
  }
}
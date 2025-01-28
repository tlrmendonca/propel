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

  /**
    * Goals: 
    * 1. Assert data is being created
    * 2. Merge of two single-node classes is 2
    * 3. Merge of two multi-node classes is the sum of the nodes
    */
  def testCountingNodes(): Unit = {
    import EGraph.EGraphOps

    val counting_nodes_analysis = new CountingNodesAnalysis()
    
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
  }

  /**
    * Goals: 
    * 1. Union two equal values (@note: deprecated)
    * 2. Assert correct data creation and modify working (on node with a child)
    * 3. Union two equivalent nodes (different children)
    * 4. Assert two different nodes can create a warning
    * 
    * Additional goal:
    * 6. Test operations
    */
  def testConstantFold(): Unit = {
    import EGraph.EGraphOps
    
    val constant_fold_analysis = new ConstantFoldAnalysis()
    
    val egraph = EGraph()
    egraph.addAnalysis(constant_fold_analysis)
    
    // (deprecated)
    // Goal 1: Union two equal values
    // println("\n*Goal 1* - Union two equal values")
    // val equalENodes @ Seq(xn,yn) = Seq(
    //   ENode(Operator("x")),
    //   ENode(Operator("y")),
    // )
    // val equalEClasses @ Seq(x, y) =
    //   equalENodes.map(egraph.add)
    
    // egraph.union(x, y)
    // egraph.rebuild()
    // // ^ verify that the union is successful

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

    // Goal 3: Union two equivalent nodes (different children)
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
    egraph.rebuild()
    // ^ verify that the union is successful

    // Goal 4: Assert two different nodes crash
    println("\n*Goal 4* - Assert two different nodes crash")
    val differentENodes @ Seq(cn,dn) = Seq(
      ENode(Operator("c")),
      ENode(Operator("d")),
    )
    val differentEClasses @ Seq(c, d) =
      differentENodes.map(egraph.add)

    egraph.union(c, d)
    egraph.rebuild()
    // ^ verify that the merge warns for an inconsistency
  }

  /**
    * Goals: 
    * 1. TBD
    */
  def testTypeFold(): Unit = {
    import EGraph.EGraphOps

    val type_fold_analysis = new TypeFoldAnalysis()

    val egraph = EGraph()
    egraph.addAnalysis(type_fold_analysis)

    val numberedENodes @ Seq(onen, twon, tn, strn) = Seq(
      ENode(Operator("1")),
      ENode(Operator("2")),
      ENode(Operator("true")),
      ENode(Operator("str")),
    )
    val numberedEClasses @ Seq(one, two, t, str) =
      numberedENodes.map(egraph.add)

    println("INITIAL STATE:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(type_fold_analysis.eclass_data.toMap))

    // +(1,4) -> Int
    val sum1n = ENode(Operator("+"), Seq(one, two))
    val sum1 = egraph.add(sum1n)

    println("\nAFTER ADDING SUM1:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(type_fold_analysis.eclass_data.toMap))

    val sum2n = ENode(Operator("+"), Seq(one, str))
    val sum2 = egraph.add(sum2n)
    // ^ type mismatch, verify warning is given

    val addersENodes @ Seq(adder1n, adder2n) = Seq(
      ENode(Operator("add1"), Seq(one)),
      ENode(Operator("add1"), Seq(two))
    )
    val addersEClasses @ Seq(adder1, adder2) = addersENodes.map(egraph.add)

    println("\nAFTER ADDING ADDERS:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(type_fold_analysis.eclass_data.toMap))
    println("adder1 type: " + type_fold_analysis.eclass_data(adder1.id))
    println("equivalent: " + AnalysisType(Seq(AnalysisType(BType.Number)), AnalysisType(BType.Number)))
  }
  
  // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisTester"
  def main(args: Array[String]): Unit = {
    println("Starting AnalysisTester...")
    testTypeFold()
    println("AnalysisTester completed.")
  }
}
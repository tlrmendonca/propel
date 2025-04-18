package propel.evaluator.egraph.mutable.simple

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.simple.analysisExamples.*

object AnalysisTester {

  def prettyPrintEClasses(eclasses: Map[EClass, Set[ENode]]): String = {
    eclasses.toSeq.sortBy(_._1.toString).map { case (eclass, enodes) =>
      s"$eclass -> ${enodes.mkString(",")}"
    }.mkString("\n")
  }
  
  def prettyPrintData[D](data: Map[EClass.Id, D]): String = {
    data.toSeq.sortBy(_._1.toString).map { case (id, data) =>
      val str = id.toString.stripPrefix("Symbol(").stripSuffix(")")
      s"$str -> $data"
    }.mkString("\n")
  }

  def printEGraphState(eclasses: Map[EClass, Set[ENode]], analysis: Analysis, message: String): Unit = {
    println(message)
    println(prettyPrintEClasses(eclasses))
    print("\n")
    println(prettyPrintData(analysis.eclass_data.toMap))
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
  }

  /**
    * Goals: 
    * 1. TBD
    */
  def testAdvConstantFoldSimplified(): Unit = {
    import EGraph.EGraphOps

    val adv_constant_fold_analysis = new AdvConstantFoldAnalysis()
    
    val egraph = EGraph()
    egraph.addAnalysis(adv_constant_fold_analysis)
    
    val numberedENodes @ Seq(onen, twon, threen, fourn) = Seq(
      ENode(Operator("1")),
      ENode(Operator("2")),
      ENode(Operator("3")),
      ENode(Operator("4")),
    )
    val numberedEClasses @ Seq(one, two, three, four) =
      numberedENodes.map(egraph.add)
      
    println("BEFORE ADDING SUM:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(adv_constant_fold_analysis.eclass_data.toMap))
    
    // +(1,4)
    val sum1n = ENode(Operator("+"), Seq(one, four))
    val sum1 = egraph.add(sum1n)
    
    println("AFTER ADDING SUM1:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(adv_constant_fold_analysis.eclass_data.toMap))
    
    // +(2,3)
    val sum2n = ENode(Operator("+"), Seq(two, three))
    val sum2 = egraph.add(sum2n)
    
    println("AFTER ADDING SUM2:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(adv_constant_fold_analysis.eclass_data.toMap))
    // ^ notice that the union is not needed to join +(2,3) to +(1,4)
  }

  /**
    * Goals: 
    * 1. TBD
    */
  def testAdvConstantFoldComplete(): Unit = {
    import EGraph.EGraphOps

    val adv_constant_fold_analysis = new AdvConstantFoldAnalysis()
    
    val egraph = EGraph()
    egraph.addAnalysis(adv_constant_fold_analysis)

    val numberedENodes @ Seq(onen, twon, threen, fourn) = Seq(
      ENode(Operator("1")),
      ENode(Operator("2")),
      ENode(Operator("3")),
      ENode(Operator("4")),
    )
    val numberedEClasses @ Seq(one, two, three, four) =
      numberedENodes.map(egraph.add)
      
    println("BEFORE OPS:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(adv_constant_fold_analysis.eclass_data.toMap))

    val sumn = ENode(Operator("+"), Seq(one, two))
    val subn = ENode(Operator("sub"), Seq(three, two))
    val muln = ENode(Operator("mul"), Seq(one, one))
    val divn = ENode(Operator("div"), Seq(three, one))
    val sqn = ENode(Operator("pow2"), Seq(two))
    val opsEClasses @ Seq(sum, sub, mul, div, sq) = 
      Seq(sumn, subn, muln, divn, sqn).map(egraph.add)
    
    println("AFTER OPS:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(adv_constant_fold_analysis.eclass_data.toMap))
    // ^ notice that there are 4 classes in total, each representing a number from 1 to 4

    egraph.union(sum, three)
    // ^ verify that the union is successful and cx={1+2,3}
    egraph.union(sub, one)
    // ^ verify that the union is successful and cx={3-2,1}
    egraph.union(mul, one)
    // ^ verify that the union is successful and cx={1*1,1}
    egraph.union(div, three)
    // ^ verify that the union is successful and cx={3/1,3}
    egraph.union(sq, four)
    // ^ verify that the union is successful and cx={2^2,4}
    egraph.rebuild()
    println("END:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(adv_constant_fold_analysis.eclass_data.toMap))
  }

  /**
    * Goals: 
    * 1. Add disunions and pass the consistency test
    * 2. Add merge that breaks consistency and assert failure in the consistency test
    */
  def testDisequalityAnalysis(): Unit = {
    val disequality_analysis = new DisequalityAnalysis()

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
    println(s"Consistency check: ${disequality_analysis.is_consistent(egraph)}")
    
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(disequality_analysis.eclass_data.toMap))

    // Goal 2
    println("\n*Goal 2* - Assert inconsistency")

    egraph.union(a, b) // illegal union
    egraph.rebuild()

    // check consistency
    println(s"Consistency check: ${disequality_analysis.is_consistent(egraph)}")

    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(disequality_analysis.eclass_data.toMap))
    // ^ note that a and b are both equal and disequal to each other
  }

  def testConstantFoldWithTypingAnalysis(): Unit = {
    val constant_fold_with_typing_analysis = new ConstantFoldWithTypingAnalysis(new TypeFoldAnalysis())

    val egraph = EGraph()
    egraph.addAnalysis(constant_fold_with_typing_analysis)

    println(s"Added Analysis's dependencies? ${egraph.getAnalysisList().map(_.getClass.getSimpleName)}\n")
    // ^ verify that the dependencies are added

    // Create test enodes
    val constantENodes @ Seq(strn, booln) = Seq(
      ENode(Operator("string")),
      ENode(Operator("true")),
    )
    val constantEClasses @ Seq(str, bool) =
      constantENodes.map(egraph.add)

    egraph.union(str, bool)
    // ^ should warn about a type mismatch by detecting global_data flag
  }

  /**
    * Goals:
    * 1. Single operation with two variables
    * 2. Nested operations
    * 3. Trying to find common CVecs with more complex functions
    */
  def testCVecAnalysis(): Unit = {
    println("Select an example to run: 1-3")
    
    val selection = scala.io.StdIn.readLine("Enter your choice (1-3): ").trim
    
    val cvec_analysis = new CVecAnalysis()
    val egraph = EGraph()
    egraph.addAnalysis(cvec_analysis)
    
    selection match {
      case "1" => 
        val eNodes @ Seq(varn, var2n) = Seq(
          ENode(Operator("x")),
          ENode(Operator("y")),
        )
        val eClasses @ Seq(varx, vary) =
          eNodes.map(egraph.add)
        
        printEGraphState(egraph.eclasses, cvec_analysis, "Initial state with just variables:")
        
        val opENodes @ Seq(op1n) = Seq(
          ENode(Operator("+"), Seq(varx, vary)),
        )
        val opEClasses @ Seq(op1) =
          opENodes.map(egraph.add)
        
        printEGraphState(egraph.eclasses, cvec_analysis, "After adding operations:")
        // ^ final cved is created correctly by adding each element of the x and y cvecs
        
      case "2" =>
        val constantENodes @ Seq(xn, yn, twon) = Seq(
          ENode(Operator("x")),
          ENode(Operator("y")),
          ENode(Operator("2")),
        )
        val constantEClasses @ Seq(x, y, two) =
          constantENodes.map(egraph.add)
          
        val op1n = ENode(Operator("+"), Seq(x, y))
        val op1 = egraph.add(op1n)
        val op2n = ENode(Operator("*"), Seq(op1, two))
        val op2 = egraph.add(op2n)

        printEGraphState(egraph.eclasses, cvec_analysis, "After adding operations:")
        // ^ final cved is created correctly by adding each element of the x and y cvecs and then squaring each one
        
      case "3" | _ =>
        // "Unintuitive" expression equivalencies
        // (x + y)² = x² + 2xy + y²
        val constantENodes @ Seq(xn, yn, twon) = Seq(
          ENode(Operator("x")),
          ENode(Operator("y")),
          ENode(Operator("2")),
        )
        val constantEClasses @ Seq(x, y, two) =
          constantENodes.map(egraph.add)
        
        // (x + y)²
        val op1n = ENode(Operator("+"), Seq(x, y))
        val op1 = egraph.add(op1n)
        val op2n = ENode(Operator("pow2"), Seq(op1))
        val op2 = egraph.add(op2n)

        // x² + 2xy + y²
        val opsENodes @ Seq(powxn, powyn, xyn) = Seq(
          ENode(Operator("pow2"), Seq(x)),
          ENode(Operator("pow2"), Seq(y)),
          ENode(Operator("*"), Seq(x, y)),
        )
        val opsEClasses @ Seq(powx, powy, xy) =
          opsENodes.map(egraph.add)
        val op3n = ENode(Operator("*"), Seq(xy, two))
        val op3 = egraph.add(op3n)
        val op4n = ENode(Operator("+"), Seq(powx, op3))
        val op4 = egraph.add(op4n)
        val op5n = ENode(Operator("+"), Seq(op4, powy))
        val op5 = egraph.add(op5n)

        printEGraphState(egraph.eclasses, cvec_analysis, "After adding second level operations:")
        // ^ search for cvecs of the classes pow2(+(x,y)) and +(+(pow2(x),*(*(x,y),2)),pow2(y))
    }
  }

  // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisTester" 
  def main(args: Array[String]): Unit = {
    println("Starting AnalysisTester...")
    testCVecAnalysis()
    println("AnalysisTester completed.")
  }
}
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
    * 1. Single operation with two variables
    * 2. Nested operations
    * 3. Trying to find common CVecs with more complex functions
    */
  def testCVecAnalysis(): Unit = {
    println("Select an example to run:" +
      "\n1. Single operation with two variables" +
      "\n2. Nested operations" +
      "\n3. Trying to find common CVecs with more complex functions" +
      "\n4. Lists and Strings")

    
    val selection = scala.io.StdIn.readLine("Enter your choice (1-4): ").trim
    
    val cvec_analysis = new CVecAnalysis(new TypeFoldAnalysis())
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
        val op2n = ENode(Operator("+"), Seq(op1, two))
        val op2 = egraph.add(op2n)

        printEGraphState(egraph.eclasses, cvec_analysis, "After adding operations:")
        // ^ final cved is created correctly by adding each element of the x and y cvecs and then squaring each one
        
      case "3" =>
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

      case "4" | _ =>
        val constantENodes @ Seq(sn, vn, ln) = Seq(
          ENode(Operator("s")),
          ENode(Operator("v")),
          ENode(Operator("l")),
        )
        val constantEClasses @ Seq(s, v, l) =
          constantENodes.map(egraph.add)
        // ^ verify that the cvecs are created according to type

        printEGraphState(egraph.eclasses, cvec_analysis, "Basic nodes added:")

        val op1n = ENode(Operator("concat"), Seq(s, v))
        val op1 = egraph.add(op1n)
        // ^ verify concatenation of strings in cvecs works fine

        printEGraphState(egraph.eclasses, cvec_analysis, "Concat s and v:")

        egraph.union(s, v)

        printEGraphState(egraph.eclasses, cvec_analysis, "After unioning before rebuild s and v:")

        egraph.rebuild()
        // ^ verify that the cvecs unify
        // ^ verify that the cvec for op1 is updated

        printEGraphState(egraph.eclasses, cvec_analysis, "After rebuild:")
    }
  }

  // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisTester" 
  def main(args: Array[String]): Unit = {
    println("Starting AnalysisTester...")
    testCVecAnalysis()
    println("AnalysisTester completed.")
  }
}
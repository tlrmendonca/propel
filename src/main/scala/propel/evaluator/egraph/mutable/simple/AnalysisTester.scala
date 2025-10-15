package propel.evaluator.egraph.mutable.simple

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.simple.analysisExamples.*
import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

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
    println(s"\n$message\n")
    println(prettyPrintEClasses(eclasses))
    print("\n")
    println(prettyPrintData(analysis.eclass_data.toMap))
  }

  def printSimilarExpressions[G](egraph: G, e1: EClass, e2: EClass, analysis: Analysis)(using EGraphOps[G]): Unit = {
    val expr1 = ExpressionExtractor.extract(egraph, e1)
    val expr2 = ExpressionExtractor.extract(egraph, e2)
    println(s"""
      |Extracted expressions:
      |  1: ${expr1.padTo(40, ' ')} CVEC: ${analysis.getData(e1.id)}
      |  2: ${expr2.padTo(40, ' ')} CVEC: ${analysis.getData(e2.id)}
      |""".stripMargin)
  }

  /**
    * Goals: 
    * 1. TBD
    */
  def testTypeFold(): Unit = {
    import EGraph.EGraphOps

    val vars_analysis = new VarsAnalysis(MutableHashMap())
    val type_fold_analysis = new TypeFoldAnalysis(vars_analysis)

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

  def testVarsAnalysis(): Unit = {
    import EGraph.EGraphOps

    val varList = MutableHashMap("x" -> LType.Number, "y" -> LType.Number, "s" -> LType.String)
    val vars_analysis = new VarsAnalysis(varList)

    val egraph = EGraph()
    egraph.addAnalysis(vars_analysis)

    val varENodes @ Seq(xn, yn, sn, constn) = Seq(
      ENode(Operator("x")),
      ENode(Operator("y")),
      ENode(Operator("s")),
      ENode(Operator("1")),
    )
    val varEClasses @ Seq(x, y, s, const) =
      varENodes.map(egraph.add)
    
    printEGraphState(egraph.eclasses, vars_analysis, "Vars Analysis:")
  }

  /**
    * Goals:
    * 1. Single operation with two variables
    * 2. Nested operations
    * 3. Trying to find common CVecs with more complex functions
    */
  def testCVecAnalysis(): Unit = {
    while (true) {
      println("Select an example to run:" +
        "\n1. Single operation with two variables" +
        "\n2. Freshmen's Dream (Confirm Inequality)" +
        "\n3. Binomial Expansion (True)" +
        "\n4. Division Simplification (Almost True)" +
        "\n5. (Broken) Lists and Strings")
      
      val selection = scala.io.StdIn.readLine("Enter your choice (1-4): ").trim
      
      val vars_analysis = new VarsAnalysis(MutableHashMap(
              "x" -> LType.Number,
              "y" -> LType.Number
          ))
      val type_analysis = new TypeFoldAnalysis(vars_analysis)
      val cvec_analysis = new CVecAnalysis(
          type_analysis,
          vars_analysis
      )
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
          // "Freshman's Dream" test
          // (x + y)² == x² + y²  (false in general, true only in characteristic 2 fields)
          val xn = ENode(Operator("x"))
          val x = egraph.add(xn)
          val yn = ENode(Operator("y"))
          val y = egraph.add(yn)

          // (x + y)²
          val sum = ENode(Operator("+"), Seq(x, y))
          val sumClass = egraph.add(sum)
          val leftExpr = ENode(Operator("pow2"), Seq(sumClass))
          val left = egraph.add(leftExpr)

          // x² + y²
          val powxn = ENode(Operator("pow2"), Seq(x))
          val powx = egraph.add(powxn)
          val powyn = ENode(Operator("pow2"), Seq(y))
          val powy = egraph.add(powyn)
          val rightExpr = ENode(Operator("+"), Seq(powx, powy))
          val right = egraph.add(rightExpr)

          printEGraphState(egraph.eclasses, cvec_analysis, "Freshman's Dream test:")
          
          printSimilarExpressions(egraph, left, right, cvec_analysis)
          // Expected: not equivalent over reals; equivalent mod 2
          
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

          printSimilarExpressions(egraph, op2, op5, cvec_analysis)

        case "4" =>
          // Division simplification edge case
          // (x² - 1) / (x - 1) == x + 1
          // True for all x ≠ 1, undefined when x = 1
          val xn = ENode(Operator("x"))
          val x = egraph.add(xn)
          val onen = ENode(Operator("1"))
          val one = egraph.add(onen)

          // (x² - 1)
          val pow2xn = ENode(Operator("pow2"), Seq(x))
          val pow2x = egraph.add(pow2xn)
          val op1n = ENode(Operator("-"), Seq(pow2x, one))
          val op1 = egraph.add(op1n)

          // (x - 1)
          val op2n = ENode(Operator("-"), Seq(x, one))
          val op2 = egraph.add(op2n)

          // (x² - 1) / (x - 1)
          val leftn = ENode(Operator("div"), Seq(op1, op2))
          val left = egraph.add(leftn)

          // x + 1
          val rightn = ENode(Operator("+"), Seq(x, one))
          val right = egraph.add(rightn)

          printEGraphState(egraph.eclasses, cvec_analysis, "Division simplification edge case:")
          val exp1 = ExpressionExtractor.extract(egraph, left)
          val exp2 = ExpressionExtractor.extract(egraph, right)
          printSimilarExpressions(egraph, left, right, cvec_analysis)
          // Expected: equivalent for all x except x = 1 (division by zero)

        case "5" | _ =>
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
      val continue = scala.io.StdIn.readLine("Run another example? (y/n): ")
      if (continue != "y") {
        println("Exiting AnalysisTester.")
        return
      }
    }
  }

  // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisTester" 
  def main(args: Array[String]): Unit = {
    println("Starting AnalysisTester...")
    testCVecAnalysis()
    println("AnalysisTester completed.")
  }
}
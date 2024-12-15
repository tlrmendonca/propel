package propel.evaluator.egraph.mutable.simple.AnalysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps, AnalysisType, BType}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

object TypeFoldAnalysis {

  def prettyPrintEClasses(eclasses: Map[EClass, Set[ENode]]): String = {
    eclasses.toSeq.sortBy(_._1.toString).map { case (eclass, enodes) =>
      s"$eclass -> ${enodes.mkString(",")}"
    }.mkString("; ")
  }

  def prettyPrintData[D](data: Map[EClass.Id, D]): String = {
    data.toSeq.sortBy(_._1.toString).map { case (id, data) =>
      val str = id.toString.stripPrefix("Symbol(").stripSuffix(")")
      s"$str -> ${data.toString}"
    }.mkString("; ")
  }

  /**
    * [[Type Folding]]
    * Level: Medium
    * Goal: Assert that all enodes respect typing relations
    */
  val type_fold_analysis = new Analysis {

      /**
        * [[Data]] set as [[String]] to simplify representation of types.
        */
      type Data = AnalysisType
      val eclass_data = MutableMap()

      /**
        * Goal: Represent the type of a node.
        *
        * @param g graph
        * @param x node
        * @return type of said node
        */
      def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
        val f = operations.getOrElse(x.op, null)
        val args = x.refs.map(ref => getData(ref.id).get)
        f match {
          case null => AnalysisType(toType(x.op))
          case f: (Function1[Seq[AnalysisType], AnalysisType], Int) =>
            if (args.length != f._2) throw new Exception("Invalid number of arguments, expected " + f._2 + ", but " + args.length + " given")
            f._1(args)
        }
      }

      /**
        * Goal: Auxiliary function to define the type of a basic [[ENode]].
        * 
        * @note "Basic" in this context refers to nodes that are not dependent on children, i.e., numbers, strings and booleans.
        *
        * @param op operator
        * @return type of said operator
        */
      def toType(op: Operator): BType = {
        op.toString match {
          case "true" | "false" => BType.Boolean
          case s if s.matches("""-?\d+(\.\d+)?""") => BType.Number
          case _ => BType.String
        }
      }

      /**
        * Goal: check if two classes have the same type.
        *
        * @param data1 data
        * @param data2 data
        * @return
        */
      def merge(data1: Data, data2: Data): Data = {
        if (data1 == data2) 
          println(s"Merge: $data1 and $data2 are the same type")
          data1
        else throw new Exception(s"Inconsistent types! Cannot merge (${data1.toString()}) and (${data2.toString()})") 
      }

      /**
        * Goal: Nothing. Can be used for checks, but in such a simple analysis those can be made in *make* and *merge* without overhead.
        *
        * @param egraph graph
        * @param id class id
        */
      def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
        return
      }
  }

  val functions = MutableHashMap[Operator, (Function1[Seq[AnalysisType], AnalysisType], Int)](
    Operator("+") -> (args => (
      if args(0).basicType == BType.Number && args(1).basicType == BType.Number
      then AnalysisType(BType.Number)
      else throw new Exception(s"Invalid types for +, given (${args(0)}) and (${args(1)})")
    ),2),
    Operator("add1") -> (args => (
      if args(0).basicType == BType.Number
      then AnalysisType(Seq(AnalysisType(BType.Number)), AnalysisType(BType.Number))
      else throw new Exception(s"Invalid types for add1, given (${args(0)})")
    ),1)
  )

  // Functions need to be added since operations is unmodifiable
  type_fold_analysis.operations ++= functions


  // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisExamples.testTypeFold"
  @main def testTypeFold(): Unit =
    import EGraph.EGraphOps

    /**
      * Goals: 
      * 1. TBD
      */
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

    val exception = try {
      val sum2n = ENode(Operator("+"), Seq(one, str))
      val sum2 = egraph.add(sum2n)
      None
    } catch {
      case e: Exception => Some(e)
    }
    assert(exception.isDefined)
    println(s"\nException reached: ${exception.get.getMessage}")
    // ^ type mismatch

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
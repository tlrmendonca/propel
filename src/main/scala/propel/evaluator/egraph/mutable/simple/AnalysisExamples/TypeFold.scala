package propel.evaluator.egraph.mutable.simple.AnalysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import java.util.HashMap

object TypeFoldAnalysis {

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
    * [[Type Folding]]
    * Level: Medium
    * Goal: Assert that all enodes respect typing relations
    */
  val type_fold_analysis = new Analysis {

      /**
        * [[Data]] set as [[String]] to simplify representation of types.
        */
      type Data = String
      val eclass_data = MutableMap()

      /**
        * Goal: Represent the type of a node.
        *
        * @param g graph
        * @param x node
        * @return type of said node
        */
      def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
        var data = ""

        val f = operations.getOrElse(x.op, null)
        val args = x.refs.map(ref => getData(ref.id).get)
        f match {
          case null => data = toType(x.op)
          case f: (Function1[Seq[String], String], Int) =>
            if (args.length != f._2) throw new Exception("Invalid number of arguments, expected " + f._2 + ", but " + args.length + " given")
            data = f._1(args)
        }
        
        return data
      }

      /**
        * Goal: Auxiliary function to define the type of a basic [[ENode]].
        * 
        * @note "Basic" in this context refers to nodes that are not dependent on children, i.e., numbers, strings and booleans.
        *
        * @param op operator
        * @return type of said operator
        */
      def toType(op: Operator): String = {
        op.toString match {
          case "true" | "false" => "Boolean"
          case s if s.matches("""-?\d+(\.\d+)?""") => "Number"
          case _ => "String" }
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
        else throw new Exception(s"Inconsistent types! Cannot merge ($data1) and ($data2)") 
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

  val functions = MutableHashMap[Operator, (Function1[Seq[String], String], Int)](
    Operator("+") -> (args => ( if args(0) == "Number" && args(1) == "Number" then "Number" else throw new Exception(s"Invalid types for +, given (${args(0)}) and (${args(1)})")), 2),
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

    val numberedENodes @ Seq(onen, twon, threen, fourn, tn, strn) = Seq(
      ENode(Operator("1")),
      ENode(Operator("2")),
      ENode(Operator("3")),
      ENode(Operator("4")),
      ENode(Operator("true")),
      ENode(Operator("str")),
    )
    val numberedEClasses @ Seq(one, two, three, four, t, str) =
      numberedENodes.map(egraph.add)
      
    println("BEFORE ADDING SUM:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(type_fold_analysis.eclass_data.toMap))

    // +(1,4) -> Int
    val sum1n = ENode(Operator("+"), Seq(one, four))
    val sum1 = egraph.add(sum1n)
    
    println("AFTER ADDING SUM1:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(type_fold_analysis.eclass_data.toMap))
    
    // +(2,+(1,4)) -> Int
    val sum2n = ENode(Operator("+"), Seq(two, sum1))
    val sum2 = egraph.add(sum2n)

    println("AFTER ADDING SUM2:")
    println(prettyPrintEClasses(egraph.eclasses))
    println(prettyPrintData(type_fold_analysis.eclass_data.toMap))
    // ^ notice that the type match

    val exception = try {
      val sum3n = ENode(Operator("+"), Seq(one, str))
      val sum3 = egraph.add(sum3n)
      None
    } catch {
      case e: Exception => Some(e)
    }
    egraph.rebuild()
    assert(exception.isDefined)
    println(s"Exception reached: ${exception.get.getMessage}")
    // ^ type mismatch
}
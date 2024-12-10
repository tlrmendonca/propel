package propel.evaluator.egraph.mutable.simple.AnalysisExamples

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.{EGraph, EGraphOps}

import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}
import java.util.HashMap

object AdvConstantFoldAnalysis {

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
    * [[Advanced Constant Folding]]
    * Level: Hard
    * Goal: Assert that all enodes deemed equal have the same constant value
    */
  val adv_constant_fold_analysis = new Analysis {

      /**
        * [[Data]] set as [[String]] to use [[ENode]]'s [[Operator]] as a constant value.
        */
      type Data = String
      val eclass_data = MutableMap()

      /**
        * Goal: Calculate the constant value if dependent on children.
        *
        * @param g graph
        * @param x node
        * @return constant value of said node
        */
      def make[G](egraph: G, x: ENode)(using EGraphOps[G]): Data = {
        
        val f = operations.getOrElse(x.op, null)
        val args = x.refs.map(ref => getData(ref.id).get)
        var data = ""
        f match {
          case null => return x.op.toString
          case f: (Function1[Seq[String], String], Int) =>
            if (args.length != f._2) throw new Exception("Invalid number of arguments, expected " + f._2 + ", but " + args.length + " given")
            data = f._1(args)
        }
          
        val xc = egraph.find(EClass(x))
        eclass_data.update(xc.id, data)

        // println(s"Make: class ${xc} added and data set to ${eclass_data(xc.id)}")
        return data
        // return ""
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
      def modify[G](egraph: G, id: EClass.Id)(using EGraphOps[G]): Unit = {
        val original_class = egraph.getEClassFromId(id)
        val data = getData(id).get
        val new_node = ENode(Operator(data)) // concretize
        val new_class = egraph.add(new_node)
        egraph.union(original_class, new_class) // this joins any two classes with the same data
      }
  }

  val functions = MutableHashMap[Operator, (Function1[Seq[String], String], Int)](
    Operator("+") -> (args => ((args(0).toInt + args(1).toInt).toString), 2),
    Operator("sub") -> (args => ((args(0).toInt - args(1).toInt).toString), 2),
    Operator("mul") -> (args => ((args(0).toInt * args(1).toInt).toString), 2),
    Operator("div") -> (args => ((args(0).toInt / args(1).toInt).toString), 2),
    Operator("pow2") -> (args => ((args(0).toInt * args(0).toInt).toString), 1),
  )

  // Functions need to be added since operations is unmodifiable
  adv_constant_fold_analysis.operations ++= functions


  // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisExamples.testAdvConstantFoldSimplified"
  @main def testAdvConstantFoldSimplified(): Unit =
    import EGraph.EGraphOps

    /**
      * Goals: 
      * 1. TBD
      */
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

  // sbt "runMain propel.evaluator.egraph.mutable.simple.AnalysisExamples.testAdvConstantFoldComplete"
  @main def testAdvConstantFoldComplete(): Unit =
    import EGraph.EGraphOps

    /**
      * Goals: 
      * 1. TBD
      */
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
    
    println("AFTER SUM:")
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
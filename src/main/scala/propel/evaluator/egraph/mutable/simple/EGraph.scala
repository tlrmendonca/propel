package propel.evaluator.egraph.mutable.simple

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.{EGraphOps, UnionFind}
import collection.mutable.{Map as MutableMap, Set as MutableSet}

/** Definition of a mutable egraph. */
object EGraph:
  /** Alias for [[empty]]. */
  def apply(): EGraph = EGraph.empty

  /** @return an empty e-graph. */
  def empty: EGraph = BasicEGraph()

  /** An e-graph data structure. */
  opaque type EGraph = BasicEGraph
  given EGraphOps: EGraphOps[EGraph] = new EGraph.BasicEGraphOps {}

  /** Basic implementation of an e-graph. */
  private case class BasicEGraph(
    underlying: UnionFind.UnionFind[EClass] = UnionFind(),
    classes: MutableMap[EClass.Id, EClass] = MutableMap(),
    enodes: MutableMap[ENode, EClass.Id] = MutableMap(),
    uses: MutableMap[EClass.Id, MutableMap[ENode, EClass]] = MutableMap(),
    worklist: MutableSet[EClass.Id] = MutableSet(),
  ):
    override def toString: String =
      s"""EGraph:
        | unionfind: $underlying
        | classes: $classes
        | enodes: $enodes
        | uses: $uses
        | worklist: $worklist
        |""".stripMargin

  /** Basic operations for egraphs. */
  private trait BasicEGraphOps extends EGraphOps[BasicEGraph]:
    extension (self: BasicEGraph) {
      override def eclasses: Map[EClass, Set[ENode]] =
        self.enodes.groupBy((x, xcId) => self.find(self.classes(xcId))).map(_ -> _.toMap.keySet)
      override def add(x: ENode): EClass =
        self.lookup(x) match
          case (_, Some(xc)) => xc
          case (x0, _) =>
            val xc0 = self.find(EClass(x0))
            x0.refs.foreach(refc =>
              self.classes.update(refc.id, refc)
              self.uses.getOrElseUpdate(refc.id, MutableMap()).addOne(x0 -> xc0)
              self.enodes.update(refc.canonical, refc.id)
            )
            self.classes.update(xc0.id, xc0)
            self.uses.update(xc0.id, MutableMap())
            self.enodes.update(x0, xc0.id)
            xc0

      override def union(xc: EClass, yc: EClass): EClass =
        val xc0 = self.find(xc)
        val yc0 = self.find(yc)
        if xc0 == yc0 then return xc0

        val xyc = self.underlying.union(xc0, yc0)
        val xycUses = self.uses.getOrElseUpdate(xyc.id, MutableMap())
        val other = if xyc.id == xc0.id then yc0 else xc0
        val otherUses = self.uses.getOrElseUpdate(other.id, MutableMap())
        xycUses.addAll(otherUses)
        self.uses.remove(other.id)

        self.worklist.add(xyc.id)
        xyc

      override def find(xc: EClass): EClass =
        self.underlying.find(xc)

      override def equal(xc: EClass, yc: EClass): Boolean =
        self.underlying.congruent(xc, yc)

      override def rebuild(): Unit =
        def repair(xc: EClass): Unit =
          val xcUses = self.uses.remove(xc.id).getOrElse(MutableMap())
          // Restore Congruence Invariance: Vf Vx Vy. x=y -> f(x)=f(y)
          xcUses.foreach((x, xcStale) =>
            self.enodes.remove(x)
            self.enodes.update(self.canonicalize(x), self.find(xcStale).id)
          )
          // Remove Duplicates
          val distinctUses = MutableMap[ENode, EClass]()
          xcUses.foreach((x, xcStale) =>
            val x0 = self.canonicalize(x)
            distinctUses.get(x0).foreach(xc => self.union(xcStale, xc))
            distinctUses.update(x0, self.find(xcStale))
          )
          self.uses.update(self.find(xc).id, distinctUses)

        while (self.worklist.nonEmpty) {
          val brokenClasses = self.worklist.map(id => self.find(self.classes(id)))
          self.worklist.clear()
          brokenClasses.foreach(repair)
        }

      override def canonicalize(x: ENode): ENode = ENode(x.op, x.refs.map(self.find))
      /**
       * @param x the specified e-node.
       * @return a pair containing the specified e-node canonicalized and
       *         its e-class if already known (i.e., if the e-node was
       *         already added in the past).
       */
      private def lookup(x: ENode): (ENode, Option[EClass]) =
        val x0 = self.canonicalize(x)
        (x0, self.enodes.get(x0).map(self.classes))

      override def disunion(xc: EClass, yc: EClass): Unit =
        throw new java.lang.UnsupportedOperationException("disunion is not supported by traditional egraphs")
      override def unequal(xc: EClass, yc: EClass): Boolean =
        throw new java.lang.UnsupportedOperationException("unequal is not supported by traditional egraphs")
      override def hasContradiction: Boolean =
        throw new java.lang.UnsupportedOperationException("traditional egraphs cannot have contradictions")
    }

// sbt "runMain propel.evaluator.egraph.mutable.simple.testEGraph"
@main def testEGraph(): Unit =
  import EGraph.EGraphOps
  val egraph = EGraph()

  /** Define the elements of your egraphs. */
  val constantsENodes @ Seq(an,bn,cn,dn,en,fn,gn,hn,in) = Seq(
    ENode(Operator("a")),
    ENode(Operator("b")),
    ENode(Operator("c")),
    ENode(Operator("d")),
    ENode(Operator("e")),
    ENode(Operator("f")),
    ENode(Operator("g")),
    ENode(Operator("h")),
    ENode(Operator("i")),
  )
  val constantsEClasses @ Seq(a, b, c, d, e, f, g, h, i) =
    constantsENodes.map(egraph.add)

  val operationsENodes @ Seq(fan, fbn) = Seq(
    ENode(Operator("f"), Seq(a)),
    ENode(Operator("f"), Seq(b)),
  )
  val operationsEClasses @ Seq(fa, fb) = operationsENodes.map(egraph.add)

  /** Encode equalities between the elements of your egraphs. */
  // Equality Set: {a=b; c=d; e=f; c=b}
  egraph.union(a, b)
  egraph.union(c, d)
  egraph.union(e, f)
  egraph.union(c, b)

  /** Rebuild egraph to restore congruence invariance. */
  // Congruence Invariance: Vf Vx Vy. x=y -> f(x)=f(y)
  // In this case, a=b -> f(a)=f(b)
  egraph.rebuild()

  println("\nCANONICALIZATION:")
  println(s"canonicalize($fan): ${egraph.canonicalize(fan)}")
  println(s"canonicalize($fbn): ${egraph.canonicalize(fbn)}")

  println("\nCANONICALS:")
  println((constantsEClasses ++ operationsEClasses).map(e => s"$e -> ${egraph.find(e)}").mkString("; "))

  println("\nECLASSES")
  println(s"eclasses: ${egraph.eclasses.map(_ -> _.mkString("{",";","}"))}")
  println(s"# eclasses: ${egraph.eclasses.size}")
  println(s"# enodes: ${egraph.eclasses.foldLeft(0)(_ + _._2.size)}")

  println("\nEQUALITY:")
  println(s"a = b: ${egraph.equal(a, b)}")
  println(s"f(a) = f(b): ${egraph.equal(fa, fb)}")
  // ^ Note that you never explicitly add the equality f(a) = f(b) to the egraph,
  //   but it is inferred from the equality a = b due to congruence invariance.

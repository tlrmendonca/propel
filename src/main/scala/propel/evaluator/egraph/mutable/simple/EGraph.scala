package propel.evaluator.egraph.mutable.simple

import propel.evaluator.egraph.*
import propel.evaluator.egraph.mutable.UnionFind
import propel.evaluator.egraph.mutable.simple.EGraphOps
import collection.mutable.{Map as MutableMap, Set as MutableSet, HashMap as MutableHashMap}

/** Definition of a mutable egraph. */
object EGraph:
  /** Alias for [[empty]]. */
  def apply(): EGraph[Analysis] = BasicEGraph(analysis = new Analysis {
    type Data = Int
    val eclass_data = MutableMap()
    def make[A <: Analysis, G[_ <: A]](egraph: G[A], enode: ENode)(using EGraphOps[A, G]): Data = 0
    def merge(data1: Data, data2: Data): Data = 0
    def modify[A <: Analysis, G[_ <: A]](egraph: G[A], id: EClass.Id)(using EGraphOps[A, G]): Unit = ()
  })

  /** Creates an [[Egraph]] given an [[Analysis]]. */
  def apply[A <: Analysis](analysis: A): EGraph[A] = BasicEGraph(analysis = analysis)

  /** An e-graph data structure. */
  opaque type EGraph[A <: Analysis] = BasicEGraph[A] // Jahrim Assistance
  given EGraphOps[A <: Analysis]: EGraphOps[A, EGraph] = new BasicEGraphOps[A] {} // Jahrim Assistance

  /** Basic implementation of an e-graph. */
  private case class BasicEGraph[A <: Analysis](
    underlying: UnionFind.UnionFind[EClass] = UnionFind(),
    classes: MutableMap[EClass.Id, EClass] = MutableMap(),
    enodes: MutableMap[ENode, EClass.Id] = MutableMap(),
    uses: MutableMap[EClass.Id, MutableMap[ENode, EClass]] = MutableMap(),
    worklist: MutableSet[EClass.Id] = MutableSet(),
    analysis: A,
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
  private trait BasicEGraphOps[A <: Analysis] extends EGraphOps[A, BasicEGraph]:
    extension (self: BasicEGraph[A]) {
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
            
            val data = self.analysis.make(self, x0)
            self.analysis.setData(xc0.id, data)
            self.analysis.modify(self, xc0.id) // Before return = Modify
            xc0

      override def union(xc: EClass, yc: EClass): EClass =
        val xc0 = self.find(xc)
        val yc0 = self.find(yc)
        if xc0 == yc0 then return xc0

        // Note: *analysis.merge* needs to happen before *underlying.union* in order to stop it in case of contradiction
        val xdata = self.analysis.getData(xc0.id)
        val ydata = self.analysis.getData(yc0.id)
        val data = self.analysis.merge(xdata.get, ydata.get) //FIXME: Unsafe
            
        val xyc = self.underlying.union(xc0, yc0)
        val xycUses = self.uses.getOrElseUpdate(xyc.id, MutableMap())
        val other = if xyc.id == xc0.id then yc0 else xc0
        val otherUses = self.uses.getOrElseUpdate(other.id, MutableMap())
        xycUses.addAll(otherUses)
        self.uses.remove(other.id)
        
        self.analysis.setData(xyc.id, data)
        self.worklist.add(xyc.id)
        self.analysis.modify(self, xyc.id) // Before return = Modify
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

          self.analysis.modify(self, xc.id) // This may break invariants
          
          // TODO: Turn this off in the future to test with a know to be correct analysis
          xcUses.foreach((x, xcStale) =>
            self.lookup(x) match
              case (_, Some(xcStale0)) =>
                val data = self.analysis.getData(xcStale0.id).get //FIXME: Unsafe
                val data2 = self.analysis.make(self, x)
                val newData = self.analysis.merge(data, data2)

                if newData != data then
                  self.analysis.setData(xcStale0.id, newData)
                  self.worklist.add(xcStale0.id)
              case _ => throw new java.lang.Exception("Something went wrong in analysis rebuilding") // FIXME: Change exception or use assert()
          )

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
        // TODO: Possible require(x0 belong to graph) => add .get below
        (x0, self.enodes.get(x0).map(self.classes))
      
      override def getEClassFromId(id: EClass.Id): EClass = self.classes(id)

      override def disunion(xc: EClass, yc: EClass): Unit =
        throw new java.lang.UnsupportedOperationException("disunion is not supported by traditional egraphs")
      override def unequal(xc: EClass, yc: EClass): Boolean =
        throw new java.lang.UnsupportedOperationException("unequal is not supported by traditional egraphs")
      override def hasContradiction: Boolean =
        throw new java.lang.UnsupportedOperationException("traditional egraphs cannot have contradictions")
    }

// sbt "runMain propel.evaluator.egraph.mutable.simple.testEGraph"
@main def testEGraph(): Unit =
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

package propel.evaluator.egraph.mutable.simple

/** Possible [[Type]]s of [[ENode]]s. */
enum BType:
  case Number, String, Boolean, Function, Unknown
  override def toString(): String = this match
    case Number => "Num"
    case String => "Str"
    case Boolean => "Bool"
    case _ => "?"

/** Representation of [[Type]] */
trait AnalysisType:
  
  def basicType: BType
  
/** Companion object of [[Type]]. */
object AnalysisType:

  def apply( basicType: BType ): AnalysisType = BasicType(basicType)

  def apply( 
    args: Seq[AnalysisType],
    ret: AnalysisType
  ): AnalysisType = FuncType(args = args, ret = ret)

  case class BasicType( var basicType: BType) extends AnalysisType { 
    override def toString(): String = basicType.toString()
  }
  /** Note that here [[args]] and [[ret]] are [[AnalysisType]], because functions can receive and return functions.
   * [[BasicType]]s are not like this since basicType is the only thing they need to store, which is always an [[BType]].
   * 
   * @note The toString() method becomes quite verbose. Perhaps there's a better alternative.
   */
  case class FuncType( var basicType: BType = BType.Function, var args: Seq[AnalysisType], var ret: AnalysisType ) extends AnalysisType {
    override def toString(): String = s"(${args.map(_.basicType).map(_.toString()).mkString(", ")}) -> ${ret.basicType.toString()}"
  }
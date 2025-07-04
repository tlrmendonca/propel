package propel.evaluator.egraph.mutable.simple

/** Possible [[Type]]s of [[ENode]]s. */
enum LType:
  case Number
  case String
  case Boolean
  case List(of: LType)
  case Function(args: Seq[LType], ret: LType)
  case Never
  override def toString(): String = this match
    case Number => "Num"
    case String => "Str"
    case Boolean => "Bool"
    case List(of) => s"List(${of.toString()})"
    case Function(args, ret) => s"(${args.map(_.toString()).mkString(", ")}) -> ${ret.toString()}"
    case _ => "?"

// Language
enum Expr:
  // first-order
  case NumExpr(value: Number)
  case StrExpr(value: String)
  case BoolExpr(value: Boolean)
  case ListExpr(elements: Seq[Expr])
  case Var(name: String)
  case FuncCall(name: Op, args: Seq[Expr])
  // higher-order
  case FuncDef(name: String, args: Seq[(String, LType)], body: Expr)
  // impossible
  case Broken

// Operators
enum Op:
  case PLUS 
  case MINUS
  case MULT
  case UNKNOWN
  // Debug: define toString
  override def toString(): String = this match
    case PLUS => "PLUS"
    case MINUS => "MINUS"
    case MULT => "MULT"
    case UNKNOWN => "?"

// ** Companion object for Op **
object Op:
  def fromString(s: String) : Op = {
    if (s == "+" || s == "add") Op.PLUS
    else if (s == "-" || s == "sub") Op.MINUS
    else if (s == "*" || s == "mul") Op.MULT
    else Op.UNKNOWN
  }

  def fromString(o: Operator) : Op = {
    Op.fromString(o.toString())
  }
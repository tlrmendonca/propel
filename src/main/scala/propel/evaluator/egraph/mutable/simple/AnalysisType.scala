package propel.evaluator.egraph.mutable.simple

import propel.evaluator.egraph.Operator

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
// NOTE: Values are a subset of Expr, i.e., those that cannot be simplified further
enum LValue:
    case NumValue(value: Double)
    case StrValue(value: String)
    case BoolValue(value: Boolean)
    case ListValue(elements: Seq[LValue])
    override def toString(): String = this match
        case NumValue(value) => s"Num($value)"
        case StrValue(value) => s"Str($value)"
        case BoolValue(value) => s"Bool($value)"
        case ListValue(elements) => s"List(${elements.mkString(", ")})"

enum LExpr:
    // first-order
    case ValueExpr(value: LValue)
    case ListExpr(elements: Seq[LExpr])
    case Var(name: String)
    case FuncCall(name: Op, args: Seq[LExpr])
    // higher-order
    case FuncDef(name: String, args: Seq[(String, LType)], body: LExpr)
    // impossible
    case Broken
    override def toString(): String = this match
        case ValueExpr(value) => s"Value($value)"
        case ListExpr(elements) => s"List(${elements.mkString(", ")})"
        case Var(name) => s"Var($name)"
        case FuncCall(name, args) => s"FuncCall($name, ${args.mkString(", ")})"
        case FuncDef(name, args, body) => s"FuncDef($name, ${args.map(_._1).mkString(", ")}, $body)"
        case Broken => "Broken" 

object LValue:
    def getValueNum(value: LValue): Double = value match
        case NumValue(value) => value
        case _ => throw new IllegalArgumentException("Expected NumValue, got: " + value)

    def getValueStr(value: LValue): String = value match
        case StrValue(value) => value
        case _ => throw new IllegalArgumentException("Expected StrValue, got: " + value)

    def getValueBool(value: LValue): Boolean = value match
        case BoolValue(value) => value
        case _ => throw new IllegalArgumentException("Expected BoolValue, got: " + value)
    
    def getElementsList(value: LValue): Seq[LValue] = value match
        case ListValue(elements) => elements
        case _ => throw new IllegalArgumentException("Expected ListValue, got: " + value)

// ──────────────────────────────────────────────────────────────
// Language definitions
// ──────────────────────────────────────────────────────────────
//
// Simple arithmetic operations:
// Plus, Minus, Mult, Div, Pow2, Sqrt
//  
// Other operations to use Strings and Lists:
// Concat (a.k.a Plus) -> very simple, just to have more types than numbers
//
// ──────────────────────────────────────────────────────────────


// Operators
enum Op:
  case PLUS 
  case MINUS
  case MULT
  case DIV
  case POW2
  case SQRT
  case UNKNOWN
  // Debug: define toString
  override def toString(): String = this match
    case PLUS => "PLUS"
    case MINUS => "MINUS"
    case MULT => "MULT"
    case DIV => "DIV"
    case POW2 => "POW2"
    case SQRT => "SQRT"
    case UNKNOWN => "?"

// ** Companion object for Op **
object Op:
  def fromString(s: String) : Op = {
    if (s == "+" || s == "add") Op.PLUS
    else if (s == "-" || s == "sub") Op.MINUS
    else if (s == "*" || s == "mul") Op.MULT
    else if (s == "/" || s == "div") Op.DIV
    else if (s == "^" || s == "pow2") Op.POW2
    else if (s == "sqrt") Op.SQRT
    else Op.UNKNOWN
  }

  def fromString(o: Operator) : Op = {
    Op.fromString(o.toString())
  }
  // FIXME: When "unkown", op shows just "?" which is not very helpful for debugging
  // Would need some way for "UNKNOWN" to carry the original string
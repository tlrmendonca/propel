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
enum Value:
    case NumValue(value: Double)
    case StrValue(value: String)
    case BoolValue(value: Boolean)
    case ListValue(elements: Seq[Value])
    override def toString(): String = this match
        case NumValue(value) => s"Num($value)"
        case StrValue(value) => s"Str($value)"
        case BoolValue(value) => s"Bool($value)"
        case ListValue(elements) => s"List(${elements.mkString(", ")})"

enum Expr:
    // first-order
    case ValueExpr(value: Value)
    case ListExpr(elements: Seq[Expr])
    case Var(name: String)
    case FuncCall(name: Op, args: Seq[Expr])
    // higher-order
    case FuncDef(name: String, args: Seq[(String, LType)], body: Expr)
    // impossible
    case Broken
    override def toString(): String = this match
        case ValueExpr(value) => s"Value($value)"
        case ListExpr(elements) => s"List(${elements.mkString(", ")})"
        case Var(name) => s"Var($name)"
        case FuncCall(name, args) => s"FuncCall($name, ${args.mkString(", ")})"
        case FuncDef(name, args, body) => s"FuncDef($name, ${args.map(_._1).mkString(", ")}, $body)"
        case Broken => "Broken" 

object Value:
    def getValueNum(value: Value): Double = value match
        case NumValue(value) => value
        case _ => throw new IllegalArgumentException("Expected NumValue, got: " + value)

    def getValueStr(value: Value): String = value match
        case StrValue(value) => value
        case _ => throw new IllegalArgumentException("Expected StrValue, got: " + value)

    def getValueBool(value: Value): Boolean = value match
        case BoolValue(value) => value
        case _ => throw new IllegalArgumentException("Expected BoolValue, got: " + value)
    
    def getElementsList(value: Value): Seq[Value] = value match
        case ListValue(elements) => elements
        case _ => throw new IllegalArgumentException("Expected ListValue, got: " + value)


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
package propel.evaluator.egraph.mutable.simple
// Type Definitions
import propel.defaults.content

enum Type:
  case Nat
  case Boolean
  case Function(arg: Seq[Type], ret: Type)

import Type.*

// Constructors Definition

enum ConstructorName:
  case Zero
  case Succ
  case True
  case False

import ConstructorName.*

// maps constructor names to their types
def constructor_type(const: ConstructorName): Type = const match {
  case Succ  => Function(Seq(Nat), Nat);
  case Zero  => Nat;
  case True  => Boolean;
  case False => Boolean
}

// No standard function definitions
// Arity of FunCall is the same as Function Type
enum Expr:
  case Var(name: String)
  case FunCall(name: String, args: Seq[Expr])
  case Constructor(name: ConstructorName, args: Seq[Expr])

import Expr.*

enum Value:
  case ValueConstructor(name: ConstructorName, args: Seq[Value])

  override def toString: String =
    this match {
      case ValueConstructor(Zero, _)  => "0"
      case ValueConstructor(True, _)  => "true"
      case ValueConstructor(False, _) => "false"
      case ValueConstructor(Succ, args) =>
        def count(v: Value): Int = v match {
          case ValueConstructor(Succ, inner) => 1 + count(inner.head)
          case ValueConstructor(Zero, _)     => 0
          case _                             => 0
        }
        count(this).toString
    }

import Value.*

/* Representation Example

  twice(Succ(Zero()))
  ===
  FunCall("twice", Seq(
    Constructor(Succ, Seq(
      Constructor(Zero, Seq())
    ))
  ))

 */

// Function Definitions

// Helpers for building expressions
val X: Expr = Var("X")
val TRUE: Expr = Constructor(True, Seq())
val FALSE: Expr = Constructor(False, Seq())
val ZERO: Expr = Constructor(Zero, Seq())
def SUCC(n: Expr): Expr = Constructor(Succ, Seq(n))

val function_rules: Map[FunCall, Expr] = Map(
  // twice
  FunCall("twice", Seq(ZERO)) -> ZERO,
  FunCall("twice", Seq(SUCC(X))) -> SUCC(SUCC(FunCall("twice", Seq(X)))),
  // half
  FunCall("half", Seq(ZERO)) -> ZERO,
  FunCall("half", Seq(SUCC(ZERO))) -> ZERO,
  FunCall("half", Seq(SUCC(SUCC(X)))) -> SUCC(FunCall("half", Seq(X))),
  // isZero
  FunCall("isZero", Seq(ZERO)) -> TRUE,
  FunCall("isZero", Seq(SUCC(X))) -> FALSE,

  // misc -> not really function definitions anymore
  FunCall("twice", Seq(FunCall("half", Seq(X)))) -> X,
  // half(twice(x))
  FunCall("half", Seq(FunCall("twice", Seq(X)))) -> X
  // note: this should be found as a lemma, but is written explicitly
  // here for reference that this is a possibility
)

val function_types: Map[String, Type] = Map(
  "twice" -> Function(Seq(Nat), Nat),
  "half" -> Function(Seq(Nat), Nat),
  "isZero" -> Function(Seq(Nat), Boolean),
  "sqrt" -> Function(Seq(Nat), Nat),
  "lessThan" -> Function(Seq(Nat, Nat), Boolean),
  "greaterThan" -> Function(Seq(Nat, Nat), Boolean),
  "equals" -> Function(Seq(Nat, Nat), Boolean),
  "and" -> Function(Seq(Boolean, Boolean), Boolean),
  "or" -> Function(Seq(Boolean, Boolean), Boolean),
  "not" -> Function(Seq(Boolean), Boolean),
  "max" -> Function(Seq(Nat, Nat), Nat),
  "min" -> Function(Seq(Nat, Nat), Nat),
  "mod" -> Function(Seq(Nat, Nat), Nat)
)

// returns the evaluated value of an expression
def eval(e: Expr): Option[Value] = {
  e match {
    case Var(_) => None
    case Constructor(name, args) =>
      val evaledArgs = args.map(eval)
      if (evaledArgs.forall(_.isDefined)) {
        Some(ValueConstructor(name, evaledArgs.flatten))
      } else {
        None
      }
    case FunCall(name, args) =>
      function_rules.iterator.flatMap { (lhs, body) =>
        unify(Map.empty, lhs, FunCall(name, args)).flatMap { bindings =>
          eval(substitute(bindings, body))
        }
      }.nextOption()
  }
}

// unify two expressions, returning a mapping from variable names to expressions
def unify(bindings: Map[String, Expr], lhs: Expr, rhs: Expr): Option[Map[String, Expr]] = {
  (lhs, rhs) match {
    case (Constructor(k1, args1), Constructor(k2, args2)) =>
      if (k1 == k2 && args1.length == args2.length) {
        args1.zip(args2).foldLeft(Option(bindings)) {
          case (Some(b), (a1, a2)) => unify(b, a1, a2)
          case (None, _) => None
        }
      } else None
    case (FunCall(f1, args1), FunCall(f2, args2)) =>
      if (f1 == f2 && args1.length == args2.length) {
        args1.zip(args2).foldLeft(Option(bindings)) {
          case (Some(b), (a1, a2)) => unify(b, a1, a2)
          case (None, _) => None
        }
      } else None
    case (Var(x), Var(y)) =>
      if (x == y) Some(bindings)
      else bindings.get(x) match {
        case Some(lhsVal) => unify(bindings, lhsVal, Var(y))
        case None => bindings.get(y) match {
          case Some(rhsVal) => unify(bindings, Var(x), rhsVal)
          case None => Some(bindings + (x -> Var(y)))
        }
      }
    case (Var(x), _) =>
      bindings.get(x) match {
        case Some(lhsVal) => unify(bindings, lhsVal, rhs)
        case None => Some(bindings + (x -> rhs))
      }
    case (_, Var(y)) =>
      unify(bindings, Var(y), lhs)
    case _ => None
  }
}

// substitute variables in an expression according to a mapping
def substitute(bindings: Map[String, Expr], body: Expr): Expr = {
  body match {
    case Var(name) => bindings.getOrElse(name, body)
    case FunCall(name, args) => FunCall(name, args.map(arg => substitute(bindings, arg)))
    case Constructor(name, args) => Constructor(name, args.map(arg => substitute(bindings, arg)))
  }
}
package propel.evaluator.egraph.mutable.simple.tests

import propel.evaluator.egraph.mutable.simple.*
import propel.evaluator.egraph.mutable.simple.Expr.*
import propel.evaluator.egraph.mutable.simple.Value.*
import propel.evaluator.egraph.mutable.simple.ConstructorName.*

object SimpleLanguageTests {

  def main(args: Array[String]): Unit = {
    println("=== Running SimpleLanguage Unify & Eval Tests ===\n")

    testUnification()
    testEvaluation()
    testComplexEvaluation()
    testListEvaluation()
    testListNonObvious()

    println("\n=== All Tests Completed ===")
  }

  def testUnification(): Unit = {
    println("--- Testing Unification ---")

    // 1. Basic matching
    val match1 = unify(Map.empty, ZERO, ZERO)
    assert(match1.isDefined, "ZERO should unify with ZERO")
    println("[Pass] ZERO ~ ZERO")

    // 2. Variable binding
    val match2 = unify(Map.empty, X, SUCC(ZERO))
    assert(match2.contains(Map("X" -> SUCC(ZERO))), "X should bind to SUCC(ZERO)")
    println("[Pass] X ~ SUCC(ZERO)")

    // 3. Conflict binding
    // plus(X, X) vs plus(0, 1) should fail
    val exprLHS = FunCall("plus", Seq(Var("X"), Var("X")))
    val exprRHS = FunCall("plus", Seq(ZERO, SUCC(ZERO)))
    val match3 = unify(Map.empty, exprLHS, exprRHS)
    assert(match3.isEmpty, "plus(X, X) should not unify with plus(0, 1)")
    println("[Pass] plus(X, X) !~ plus(0, 1)")

    // 4. Recursive unification
    val match4 = unify(Map.empty, SUCC(X), SUCC(SUCC(ZERO)))
    assert(match4.contains(Map("X" -> SUCC(ZERO))), "Nested X should bind to SUCC(ZERO)")
    println("[Pass] SUCC(X) ~ SUCC(SUCC(0))")

    println()
  }

  def testEvaluation(): Unit = {
    println("--- Testing Evaluation ---")

    // 1. Constructor evaluation
    val val1 = eval(SUCC(ZERO))
    assert(val1.contains(ValueConstructor(Succ, Seq(ValueConstructor(Zero, Seq())))), "SUCC(ZERO) should evaluate to Value")
    assert(val1.get.toString == "1", s"Expected '1', got ${val1.get}")
    println(s"[Pass] eval(SUCC(ZERO)) = ${val1.get}")

    // 2. Basic function: isZero
    val val2 = eval(FunCall("isZero", Seq(ZERO)))
    assert(val2.contains(ValueConstructor(True, Seq())), "isZero(0) should be true")
    println(s"[Pass] eval(isZero(0)) = ${val2.get}")

    val val3 = eval(FunCall("isZero", Seq(SUCC(ZERO))))
    assert(val3.contains(ValueConstructor(False, Seq())), "isZero(1) should be false")
    println(s"[Pass] eval(isZero(1)) = ${val3.get}")

    // 3. Recursive function: half
    // half(2) -> 1
    val val4 = eval(FunCall("half", Seq(SUCC(SUCC(ZERO)))))
    assert(val4.get.toString == "1", s"half(2) should be 1, got ${val4.get}")
    println(s"[Pass] eval(half(2)) = ${val4.get}")

    // half(3) -> 1
    val val5 = eval(FunCall("half", Seq(SUCC(SUCC(SUCC(ZERO))))))
    assert(val5.get.toString == "1", s"half(3) should be 1, got ${val5.get}")
    println(s"[Pass] eval(half(3)) = ${val5.get}")

    println()
  }

  def testComplexEvaluation(): Unit = {
    println("--- Testing Complex Examples ---")

    // twice(half(4)) -> 4
    val expr = FunCall("twice", Seq(FunCall("half", Seq(SUCC(SUCC(SUCC(SUCC(ZERO))))))))
    val result = eval(expr)
    assert(result.get.toString == "4", s"twice(half(4)) should be 4, got ${result.get}")
    println(s"[Pass] eval(twice(half(4))) = ${result.get}")

    // simplified complex composition: half(twice(1))
    val simpleComplexExpr = FunCall("half", Seq(
      FunCall("twice", Seq(SUCC(ZERO)))
    ))
    val simpleComplexResult = eval(simpleComplexExpr)
    assert(simpleComplexResult.get.toString == "1", s"half(twice(1)) should be 1, got ${simpleComplexResult.get}")
    println(s"[Pass] eval(half(twice(1))) = ${simpleComplexResult.get}")

    println()
  }

  def testListEvaluation(): Unit = {
    println("--- Testing List Evaluation ---")

    // NIL evaluates to []
    val nilVal = eval(NIL)
    assert(nilVal.isDefined, "NIL should evaluate")
    assert(nilVal.get.toString == "[]", s"NIL should be [], got ${nilVal.get}")
    println(s"[Pass] eval(NIL) = ${nilVal.get}")

    // CONS(0, NIL) evaluates to 0 :: []
    val consVal = eval(CONS(ZERO, NIL))
    assert(consVal.isDefined, "CONS(0, NIL) should evaluate")
    assert(consVal.get.toString == "0 :: []", s"CONS(0, NIL) should be '0 :: []', got ${consVal.get}")
    println(s"[Pass] eval(CONS(0, NIL)) = ${consVal.get}")

    // length(NIL) = 0
    val len0 = eval(FunCall("length", Seq(NIL)))
    assert(len0.get.toString == "0", s"length([]) should be 0, got ${len0.get}")
    println(s"[Pass] eval(length([])) = ${len0.get}")

    // length([0]) = 1
    val len1 = eval(FunCall("length", Seq(CONS(ZERO, NIL))))
    assert(len1.get.toString == "1", s"length([0]) should be 1, got ${len1.get}")
    println(s"[Pass] eval(length([0])) = ${len1.get}")

    // length([0, 1]) = 2
    val len2 = eval(FunCall("length", Seq(CONS(ZERO, CONS(SUCC(ZERO), NIL)))))
    assert(len2.get.toString == "2", s"length([0, 1]) should be 2, got ${len2.get}")
    println(s"[Pass] eval(length([0, 1])) = ${len2.get}")

    // append(NIL, [0]) = [0]
    val app1 = eval(FunCall("append", Seq(NIL, CONS(ZERO, NIL))))
    assert(app1.get.toString == "0 :: []", s"append([], [0]) should be '0 :: []', got ${app1.get}")
    println(s"[Pass] eval(append([], [0])) = ${app1.get}")

    // append([0], [1]) = [0, 1]
    val app2 = eval(FunCall("append", Seq(CONS(ZERO, NIL), CONS(SUCC(ZERO), NIL))))
    assert(app2.get.toString == "0 :: 1 :: []", s"append([0], [1]) should be '0 :: 1 :: []', got ${app2.get}")
    println(s"[Pass] eval(append([0], [1])) = ${app2.get}")

    println()
  }

  // Non-obvious: length(append([0, 1], [2])) requires evaluating append recursively
  // to get [0, 1, 2], then length recursively to get 3 — neither step is direct.
  def testListNonObvious(): Unit = {
    println("--- Testing Non-Obvious List Problem ---")

    val list01 = CONS(ZERO, CONS(SUCC(ZERO), NIL))
    val list2  = CONS(SUCC(SUCC(ZERO)), NIL)
    val expr   = FunCall("length", Seq(FunCall("append", Seq(list01, list2))))
    val result = eval(expr)
    assert(result.get.toString == "3", s"length(append([0,1], [2])) should be 3, got ${result.get}")
    println(s"[Pass] eval(length(append([0,1], [2]))) = ${result.get}")

    println()
  }
}

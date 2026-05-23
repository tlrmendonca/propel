import re

with open('src/main/scala/propel/evaluator/egraph/mutable/simple/tests/AnalysisTester.scala', 'r') as f:
    text = f.read()

# Replace the loop start
text = re.sub(r'while\s*\(\s*true\s*\)\s*\{', 'for (sel <- 1 to 5) {', text)

# Replace the first readLine
text = re.sub(r'val selection = scala\.io\.StdIn\.readLine[^\n]*\.trim', 'val selection = sel.toString\n      println("Enter your choice (1-6): " + selection)', text)

# Replace the second readLine
text = re.sub(r'val again = scala\.io\.StdIn\.readLine[^\n]*\.trim\.toLowerCase\n\s*if\s*\(again\s*!=\s*"y"\)\s*return', '', text)

with open('src/main/scala/propel/evaluator/egraph/mutable/simple/tests/AnalysisTester.scala', 'w') as f:
    f.write(text)

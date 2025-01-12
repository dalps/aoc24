from z3 import *
from collections import namedtuple
from pyparsing import *

Point = namedtuple("Point", ["x", "y"])
Round = namedtuple("Round", ["va", "vb", "prize"])

integer = Word(nums)
movement = Suppress("Button " + (Literal("A") ^ Literal("B")) +
                    ": X+") + integer("x") + Suppress(", Y+") + integer("y")
prize = Suppress("Prize: X=") + integer("px") + \
    Suppress(", Y=") + integer("py")
expr = Group(movement("a") + movement("b") + prize("p"))
p = OneOrMore(expr)


def solve(input, discrepancy):
  machines = p.parseFile(input)

  expense = 0
  rounds = []
  for m in machines:
    round = Round(va=Point(int(m["a"][0]), int(m["a"][1])),
                  vb=Point(int(m["b"][0]), int(m["b"][1])),
                  prize=Point(int(m["p"][0]), int(m["p"][1])))
    rounds.append(round)

    na = Int("na")
    nb = Int("nb")
    c = IntVal(discrepancy)

    s = Solver()

    s.add(round.va.x * na + round.vb.x * nb == round.prize.x + c)
    s.add(round.va.y * na + round.vb.y * nb == round.prize.y + c)
    # s.add(na <= 100, nb <= 100)

    print("Printing the assertions...")
    for c in s.assertions():
      print(c)

    print("Solving constraints in the solver s...")
    print(s.check())

    print(expense)
    if s.check() == sat:
      m = s.model()

      expense += (m[na].as_long() * 3 + m[nb].as_long())

      print("Traversing model...")
      for d in m.decls():
        print("%s = %s" % (d.name(), m[d]))

    print("---")

  return expense


solve1 = solve("day13.input", 0)
solve2 = solve("day13.input", 10000000000000)

print(solve1, solve2)

import pyparsing as pp
from collections import namedtuple

greet = pp.Word(pp.alphas) + "," + pp.Word(pp.alphas) + "!"

hello = "Hello, World!"

print(hello, "->", greet.parseString(hello))

input = "Button A: X+94, Y+34"

Vec2 = namedtuple("Vec2", ["x", "y"])
Round = namedtuple("Round", ["va", "vb", "prize"])

integer = pp.Word(pp.nums)
movement = pp.Suppress("Button ") + (pp.Literal("A") ^ pp.Literal("B")) + pp.Suppress(": X+") + pp.Group(integer("x") + pp.Suppress(", Y+") + integer("y"))
prize = "Prize" + pp.Group(pp.Suppress(": X=") + integer("px") + pp.Suppress(", Y=") + integer("py"))
expr = pp.Group(movement("a") + movement("b") + prize("p"))
p = pp.OneOrMore(expr)

result = p.parseFile("day13.input")

rounds = []
for r in result:
  round = Round(va=Vec2(r["a"][1][0], r["a"][1][1]),
       vb=Vec2(r["b"][1][0], r["b"][1][1]),
       prize=Vec2(r["p"][1][0], r["p"][1][1]))
  rounds.append(round)

print(rounds)
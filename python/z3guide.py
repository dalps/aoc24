from z3 import *

x = Int('x')
y = Int('y')

s = Solver()
print (s)

s.add(x > 10, y == x + 2)
print (s)
print ("Solving constraints in the solver s ...")
print (s.check())

print ("Create a new scope...")
s.push()
s.add(y < 11)
print (s)
print ("Solving updated set of constraints...")
print (s.check())

print ("Restoring state...")
s.pop()
print (s)
print ("Solving restored set of constraints...")
print (s.check())

x = Real('x')
s.add(2**x == 3)
print(s.check())

x = BitVec('x', 16)
y = BitVec('y', 16)
print(x + 2)
print((x + 2).sexpr())
a = BitVecVal(2**16 - 1, 16)
b = BitVecVal(-1, 16)
print(simplify(a == b))
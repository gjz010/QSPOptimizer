import cirq
from opt import *
c=cirq.Circuit()

l=cirq.LineQubit
for i in range(0,100):
	c.append(cirq.CZ(l(i),l(i+1)))
	c.append(cirq.H(l(i+1)))
	c.append(cirq.CZ(l(i),l(i+1)))
	c.append(cirq.H(l(i)))
	c.append(cirq.CZ(l(i),l(i+1)))


c2=optimize(c)
print(len(c2))

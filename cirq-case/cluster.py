import cirq
from opt import *
c=cirq.Circuit()

q=cirq.LineQubit
for i in range(0,200):
	c.append(cirq.H(q(3*i)))
	c.append(cirq.H(q(3*i+1)))
	c.append(cirq.H(q(3*i+2)))
	c.append(cirq.CZ(q(3*i),q(3*i+1)))
	c.append(cirq.CZ(q(3*i+1),q(3*i+2)))
	c.append(cirq.CZ(q(3*i-3),q(3*i)))
	c.append(cirq.CZ(q(3*i-2),q(3*i+1)))
	c.append(cirq.CZ(q(3*i-1),q(3*i+2)))

c2=optimize(c)
print(len(c2))

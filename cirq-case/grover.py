import cirq
from opt import *
c=cirq.Circuit()

s0=cirq.NamedQubit('s0');
s1=cirq.NamedQubit('s1');
a=cirq.NamedQubit('a');

def toffoli(s0,s1,a):
	l=[cirq.H(a),
	cirq.CNOT(s0,a),
	(cirq.T**-1).on(a),
	cirq.CNOT(s1,a),
	cirq.T(a),
	cirq.CNOT(s0,a),
	(cirq.T**-1).on(a),
	cirq.CNOT(s1,a),
	cirq.T(a),
	(cirq.T**-1).on(s0),
	cirq.H(a),
	cirq.CNOT(s1,s0),
	(cirq.T**-1).on(s0),
	cirq.CNOT(s1,s0),
	cirq.S(s0),
	cirq.T(s1)]
	return l

for i in range(1,100):
	l=toffoli(s0,s1,a)+[ # Grover Iterator
	cirq.H(s0),
	cirq.H(s1),
	cirq.X(s0),
	cirq.X(s1),
	cirq.CZ(s0,s1),
	cirq.X(s0),
	cirq.X(s1),
	cirq.H(s0),
	cirq.H(s1)]
c2=optimize(c)
print(len(c2))

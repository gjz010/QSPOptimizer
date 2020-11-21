import cirq
from opt import *
#c=cirq.Circuit()

#s0=cirq.NamedQubit('s0');
#s1=cirq.NamedQubit('s1');
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
"""
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
	c.append(l)
"""
"""
print(len(c))
#print(c)
c2=optimize(c)
print(len(c2))
#print(c2)
"""
c=cirq.Circuit()


s0=cirq.NamedQubit("s0");
s1=cirq.NamedQubit("s1");
s2=cirq.NamedQubit("s2");
s3=cirq.NamedQubit("s3");
c0=cirq.NamedQubit("c0");
c1=cirq.NamedQubit("c1");
c2=cirq.NamedQubit("c2");
c3=cirq.NamedQubit("c3");
c4=cirq.NamedQubit("c4");
c5=cirq.NamedQubit("c5");

for i in range(1,1001):
	l=[cirq.CNOT(s0,c0),
	cirq.CNOT(s1,c0),
	cirq.CNOT(s0,c1),
	cirq.CNOT(s2,c1),
	cirq.CNOT(s1,c2),
	cirq.CNOT(s3,c2),
	cirq.CNOT(s2,c3),
	cirq.CNOT(s3,c3)]+(
	toffoli(c0,c1,c4)+(
	toffoli(c2,c3,c5))+(
	toffoli(c4,c5,a))+(
	toffoli(c0,c1,c4))+(
	toffoli(c2,c3,c5)))+[
	cirq.CNOT(s0,c0),
	cirq.CNOT(s1,c0),
	cirq.CNOT(s0,c1),
	cirq.CNOT(s2,c1),
	cirq.CNOT(s1,c2),
	cirq.CNOT(s3,c2),
	cirq.CNOT(s2,c3),
	cirq.CNOT(s3,c3),
	cirq.H(s0),cirq.H(s1),cirq.H(s2),cirq.H(s3),
	cirq.X(s0),
	cirq.X(s1),
	cirq.X(s2),
	cirq.X(s3)]+(
	toffoli(s1,s2,c0))+[
	cirq.H(s0)]+(
	toffoli(s3,c0,s0))+[
	cirq.H(s0)]+(
	toffoli(s1,s2,c0))+[
	cirq.H(s0)]+(
	toffoli(s3,c0,s0))+[
	cirq.H(s0),
	cirq.X(s0),
	cirq.X(s1),
	cirq.X(s2),
	cirq.X(s3),
	cirq.H(s0),
	cirq.H(s1),
	cirq.H(s2),
	cirq.H(s3)]
	c.append(l)

print(len(optimize(c)))

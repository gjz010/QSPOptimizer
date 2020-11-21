import cirq
from opt import *
c=cirq.Circuit()

s0=cirq.NamedQubit('s0');
s1=cirq.NamedQubit('s1');
s2=cirq.NamedQubit('s2');
s3=cirq.NamedQubit('s3');
s4=cirq.NamedQubit('s4');

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

def ub(src,target):
	return [cirq.CNOT(src,target),cirq.rz(1).on(target),cirq.CNOT(src,target)]
def uc(target):
	return [cirq.rx(1).on(target)]
for i in range(0,201):
	c.append((ub(s1,s0))+(
	ub(s1,s2))+(
	ub(s1,s3))+(
	ub(s2,s0))+(
	ub(s2,s3))+(
	uc(s0))+(
	uc(s1))+(
	uc(s2))+(
	uc(s3)))
c2=optimize(c)
print(len(c2))

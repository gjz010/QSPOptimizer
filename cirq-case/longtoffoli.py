import cirq
from opt import *
c=cirq.Circuit()

l=cirq.LineQubit
a=lambda x:cirq.LineQubit(x+1000)
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
for i in range(0,100):
	c.append(toffoli(l(i),a(i),a(i+1)))


c2=optimize(c)
print(len(c2))

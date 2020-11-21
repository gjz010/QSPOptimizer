import cirq
from opt import *
c=cirq.Circuit()

s0=cirq.NamedQubit('s0');
s1=cirq.NamedQubit('s1');
s2=cirq.NamedQubit('s2');
s3=cirq.NamedQubit('s3');
s4=cirq.NamedQubit('s4');
s5=cirq.NamedQubit('s5');
s6=cirq.NamedQubit('s6');
s7=cirq.NamedQubit('s7');
s8=cirq.NamedQubit('s8');
s9=cirq.NamedQubit('s9');
s10=cirq.NamedQubit('s10');
s11=cirq.NamedQubit('s11');

s=[s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11]

def ub(src,target):
	return [cirq.CNOT(src,target),cirq.rz(1).on(target),cirq.CNOT(src,target)]
def uc(target):
	return [cirq.rx(1).on(target)]
for i in range(0,1000):
	c.append(list(map(lambda k: ub(s[k[0]],s[k[1]]), [[0,3],[2,7],[0,1],[2,3],[6,7],[1,4],[3,8],[3,4],[7,8],[4,9],[4,5],[8,9],[5,10],[9,10],[10,11]] )))
	c.append(list(map(lambda i: uc(s[i]),range(12))))
c2=optimize(c)
print(len(c2))

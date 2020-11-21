#include "stdlib.inc"

qubit q[2];
for i in 0 to 100 {
    H q[50];
    cz q[i],q[i+1];
    H q[0];
}


for i in 0 to 48 {
    H q[50];
    cz q[i],q[i+1];
    H q[50];
}
for i in 0 to 49 {
    H q[50];
    cz q[i],q[i+1];
    H q[50];
}
for i in 0 to 50 {
    H q[50];
    cz q[i],q[i+1];
    H q[50];
}

for i in 51 to 100 {
    H q[50];
    cz q[i],q[i+1];
    H q[50];
}
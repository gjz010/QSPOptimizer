#include "stdlib.inc"

qubit q[101];

for i in 0 to 99{
    cz q[i],q[i+1];
    H q[i+1];
    cz q[i],q[i+1];
    H q[i];
    cz q[i],q[i+1];
}
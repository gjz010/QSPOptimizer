#include "stdlib.inc"

qubit q[200];
for i in 0 to 99{
    cz q[i],q[i+1];
    cz q[i+1],q[i+3];
    cz q[i+3],q[i+6];
}
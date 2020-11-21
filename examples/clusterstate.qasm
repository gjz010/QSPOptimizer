#include "stdlib.inc"

qubit q[200];
/*
for i in 0 to 9 {
    H q[i];
}
*/
for i in 1 to 200 {
    /*
    for j in 0 to 9{
        H q[10*i+j];
    }
    for j in 0 to 9{
        cz q[10*i+j-10], q[10*i+j];
    }
    */
    H q[3*i];H q[3*i+1];H q[3*i+2];/*H q[10*i+3];H q[10*i+4];H q[10*i+5];H q[10*i+6];H q[10*i+7];H q[10*i+8];H q[10*i+9];*/
    cz q[3*i],q[3*i+1];
    cz q[3*i+1],q[3*i+2];
    cz q[3*i-3],q[3*i];
    cz q[3*i-2],q[3*i+1];
    cz q[3*i-1],q[3*i+2];
    /*
    cz q[10*i-7],q[10*i+3];
    cz q[10*i-6],q[10*i+4];
    cz q[10*i-5],q[10*i+5];
    cz q[10*i-4],q[10*i+6];
    cz q[10*i-3],q[10*i+7];
    cz q[10*i-2],q[10*i+8];
    cz q[10*i-1],q[10*i+9];
    */
}
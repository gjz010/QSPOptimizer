#include "stdlib.inc"

qubit space[4];
qubit c[6];
qubit ancilla[1];
/*
for i in 0..1 {
    H space[i];
}

X ancilla[0];
H ancilla[0];
*/
for i in 1 to 1000 {
    cnot(space[0],c[0]);
    cnot(space[1],c[0]);
    cnot(space[0],c[1]);
    cnot(space[2],c[1]);
    cnot(space[1],c[2]);
    cnot(space[3],c[2]);
    cnot(space[2],c[3]);
    cnot(space[3],c[3]);
    Toffoli(c[0],c[1],c[4]);
    Toffoli(c[2],c[3],c[5]);
    Toffoli(c[4],c[5],ancilla[0]);
    Toffoli(c[0],c[1],c[4]);
    Toffoli(c[2],c[3],c[5]);
    cnot(space[0],c[0]);
    cnot(space[1],c[0]);
    cnot(space[0],c[1]);
    cnot(space[2],c[1]);
    cnot(space[1],c[2]);
    cnot(space[3],c[2]);
    cnot(space[2],c[3]);
    cnot(space[3],c[3]);
    /* Grover Iterator */
    H space[0];
    H space[1];
    H space[2];
    H space[3];
    X space[0];
    X space[1];
    X space[2];
    X space[3];
    /* CZ(space[1],space[2],space[3],space[0])*/
    Toffoli(space[1],space[2],c[0]);
    H space[0];
    Toffoli(space[3],c[0],space[0]);
    H space[0];
    Toffoli(space[1],space[2],c[0]);
    H space[0];
    Toffoli(space[3],c[0],space[0]);
    H space[0];
    X space[0];
    X space[1];
    X space[2];
    X space[3];
    H space[0];
    H space[1];
    H space[2];
    H space[3];
}
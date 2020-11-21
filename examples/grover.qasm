#include "stdlib.inc"

qubit space[2];
qubit ancilla[1];
/*
for i in 0..1 {
    H space[i];
}

X ancilla[0];
H ancilla[0];
*/
#define ancilla ancilla[0]
for i in 1 to 99 {
    /* Oracle */
    //X space[0];
    //X space[1];
    H ancilla;
    cnot(space[0],ancilla);
    InvT ancilla;
    cnot(space[1],ancilla);
    T ancilla;
    cnot(space[0],ancilla);
    InvT ancilla;
    cnot(space[1], ancilla);
    T ancilla;
    InvT space[0];
    H ancilla;
    cnot(space[1], space[0]);
    InvT space[0];
    cnot(space[1], space[0]);
    S space[0];
    T space[1];
    /* Grover Iterator */
    H space[0];
    H space[1];
    X space[0];
    X space[1];
    cz space[0],space[1];
    X space[0];
    X space[1];
    H space[0];
    H space[1];
}
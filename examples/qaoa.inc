#ifndef QAOA_N_VERT
#error QAOA_N_VERT not defined when using QAOA header!
#endif

#ifndef QAOA_N_EDGE
#error QAOA_N_EDGE not defined when using QAOA header!
#endif

defgate QAOA_RB=general;
defgate QAOA_RC=general;

qubit qaoa_space[QAOA_N_VERT];

#define UB(i,t,q1,q2) cnot(qaoa_space[q1],qaoa_space[q2]);unitary(QAOA_RC[i*QAOA_N_EDGE+t]) qaoa_space[q2];cnot(qaoa_space[q1],qaoa_space[q2])
#define UC(i,q) unitary(QAOA_RC[i*QAOA_N_VERT+q]) qaoa_space[q]
#include "stdlib.inc"

/*
Using Example from Google-Cirq textbook.
*/

#define QAOA_N_VERT 12
#define QAOA_N_EDGE 15

#include "qaoa.inc"

for i in 0 to 999 {
  UB(i,0,0,3);
  UB(i,1,2,7);
  UB(i,2,0,1);
  UB(i,3,2,3);
  UB(i,4,6,7);
  UB(i,5,1,4);
  UB(i,6,3,8);
  UB(i,7,3,4);
  UB(i,8,7,8);
  UB(i,9,4,9);
  UB(i,10,4,5);
  UB(i,11,8,9);
  UB(i,12,5,10);
  UB(i,13,9,10);
  UB(i,14,10,11);
  UC(i,0);
  UC(i,1);
  UC(i,2);
  UC(i,3);
  UC(i,4);
  UC(i,5);
  UC(i,6);
  UC(i,7);
  UC(i,8);
  UC(i,9);
  UC(i,10);
  UC(i,11);
}
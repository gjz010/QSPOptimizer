#include "stdlib.inc"

/*
Using Example from Google-Cirq textbook.
*/

#define QAOA_N_VERT 12
#define QAOA_N_EDGE 15

#include "qaoa.inc"

for i in 0 to 999 {
  UB(i,0,2,1);
  UB(i,1,2,3);
  UB(i,2,4,6);
  UB(i,3,8,7);
  UB(i,4,8,11);
  //color
  UB(i,5,7,10);
  UB(i,6,9,1);
  UB(i,7,4,3);
  UB(i,8,4,5);
  UB(i,9,8,2);
  //green
  UB(i,10,7,6);
  UB(i,11,1,0);
  UB(i,12,8,9);
  UB(i,13,10,11);
  UB(i,14,7,3);
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
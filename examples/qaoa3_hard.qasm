#include "stdlib.inc"

/*
Topology:

0           1
 \         /
  2 ----- 3
 /         \
4           5
*/

#define QAOA_N_VERT 6
#define QAOA_N_EDGE 5

#include "qaoa.inc"

for i in 0 to 1000 {
  UB(i,0,0,2);
  UB(i,1,2,3);
  UB(i,2,3,1);
  UB(i,3,2,4);
  UB(i,4,3,5);
  UC(i,0);
  UC(i,1);
  UC(i,2);
  UC(i,3);
  UC(i,4);
  UC(i,5);
}
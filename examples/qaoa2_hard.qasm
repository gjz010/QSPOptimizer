#include "stdlib.inc"

/*
Topology:

      0
    /   \
  1 ----- 2
 /         \
3           4
*/

#define QAOA_N_VERT 5
#define QAOA_N_EDGE 5

#include "qaoa.inc"

for i in 0 to 1000 {
  UB(i,0,3,1);
  UB(i,1,1,0);
  UB(i,2,0,2);
  UB(i,3,2,4);
  UB(i,4,1,2);
  UC(i,0);
  UC(i,1);
  UC(i,2);
  UC(i,3);
  UC(i,4);
}
#include "stdlib.inc"

/*
Topology:

     0
   /   \
 1 ----- 2
   \   /
     3
*/

#define QAOA_N_VERT 4
#define QAOA_N_EDGE 5

#include "qaoa.inc"

for i in 0 to 200 {
  UB(i,0,1,0);
  UB(i,1,1,2);
  UB(i,2,1,3);
  UB(i,3,2,0);
  UB(i,4,2,3);
  UC(i,0);
  UC(i,1);
  UC(i,2);
  UC(i,3);
}
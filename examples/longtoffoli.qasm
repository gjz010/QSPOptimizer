#include "stdlib.inc"

qubit c[200];
qubit a[200];
X a[0];
for i in 0 to 99{
    Toffoli(c[i],a[i],a[i+1]);
}
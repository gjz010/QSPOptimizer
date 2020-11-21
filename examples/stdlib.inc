#define E 2.718281828
#define PI 3.1415926535
#define S2 0.7071067811865476
#define IS2 -0.7071067811865476
#define H unitary([S2,S2,S2,IS2])
#define X unitary([0,1,1,0])
#define S unitary([1,0,0,0+1j])
#define T unitary([1,0,0,0.7071067811865476+0.7071067811865475j])
#define InvT unitary([1,0,0,0.7071067811865476-0.7071067811865475j])
#define cnot(a,b) H b; cz a,b; H b
#define Toffoli(a,b,c)\
    H c;\
    cnot(a,c);\
    InvT c;\
    cnot(b,c);\
    T c;\
    cnot(a,c);\
    InvT c;\
    cnot(b,c);\
    T c;\
    InvT a;\
    H c;\
    cnot(b, a);\
    InvT a;\
    cnot(b, a);\
    S a;\
    T b

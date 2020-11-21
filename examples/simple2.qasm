defgate a=general;
defgate b=general;
defgate c=general;
defgate d=general;
qubit space[1];
for i in 1 to 100 {
	unitary(a[i]) space[i];
	unitary(b[i+1]) space[i+1];
	unitary(c[i+2]) space[i+2];
	unitary(d[i+3]) space[i+3];
}

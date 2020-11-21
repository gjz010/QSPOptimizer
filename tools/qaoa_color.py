from z3 import *
def try_coloring(edges, maximal_color):
	s=Solver()
	edge_directions = BoolVector("dir",(len(edges)))
	edge_colors=IntVector("color",(len(edges)))
	def check_edge(solver,i,j):
		if edges[i][0]==edges[j][0]:
			solver.add(Implies(Or(edge_directions[i], edge_directions[j]), edge_colors[i]!=edge_colors[j]))
		if edges[i][0]==edges[j][1]:
			solver.add(Implies(Or(edge_directions[i], Not(edge_directions[j])), edge_colors[i]!=edge_colors[j]))
		if edges[i][1]==edges[j][0]:
			solver.add(Implies(Or(Not(edge_directions[i]), edge_directions[j]), edge_colors[i]!=edge_colors[j]))
		if edges[i][1]==edges[j][1]:
			solver.add(Implies(Or(Not(edge_directions[i]), Not(edge_directions[j])), edge_colors[i]!=edge_colors[j]))
	for i in range(len(edges)):
		for j in range(i+1, len(edges)):
			pass
			check_edge(s, i, j)
	for i in range(len(edges)):
		s.add(edge_colors[i]>=0, edge_colors[i]<maximal_color)
	if s.check()==sat:
		print(s.model())
		return s.model()
	else:
		print(s.check())
		return None

graph=[((2,3),(2,4)),((2,4),(2,5)),((2,5),(2,6)),((2,6),(2,7)),((2,7),(2,8)),((1,7),(1,6)),((1,6),(1,5)),
((1,5),(1,4)),((0,6),(0,5)),((1,7),(2,7)),((1,6),(2,6)),((1,5),(2,5)),((1,5),(0,5)),((1,4),(2,4)),((1,6),(0,6))
]

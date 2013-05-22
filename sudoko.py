
#n = 4
#n_sub = 2
#Problem = [ [ 0 for _ in range(0,n) ] for _ in range(0,n) ]
#Problem = [ [ 0,3,0,0],[4,0,0,0],[0,4,0,0],[0,0,2,0]]
n = 9
n_sub = 3
#Problem = [ [ 0 for _ in range(0,n) ] for _ in range(0,n) ]
Problem = [ \
		[ 0,3,0,8,0,0,1,0,0],\
		[ 0,0,0,7,0,0,0,0,0],\
		[ 0,6,2,0,4,3,0,7,0],\
		[ 0,0,0,0,0,0,0,6,7],\
		[ 3,0,6,0,0,0,5,0,2],\
		[ 8,4,0,0,0,0,0,0,0],\
		[ 0,9,0,3,5,0,7,1,0],\
		[ 0,0,0,0,0,2,0,0,0],\
		[ 0,0,7,0,0,9,0,5,0],\
		]


Solution = [ [ 0 for _ in range(0,n) ] for _ in range(0,n) ]

C = [ set([ i+1 for i in range(0,n)] ) for _ in range(0,n) ]
R = [ set([ i+1 for i in range(0,n)]) for _ in range(0,n) ]
B = [ set([ i+1 for i in range(0,n)]) for _ in range(0,n) ]

Cinit = [ set() for _ in range(0,n) ]
Rinit = [ set() for _ in range(0,n) ]
Binit = [ set() for _ in range(0,n) ]
BadGuesses = set()

def block_index(i,j):
	bi = i//n_sub
	bj = j//n_sub
	return n_sub*bi+bj

for i in range(0,n):
	for j in range(0,n):
		x = Problem[i][j]
		Solution[i][j] = x
		if x != 0:
			Cinit[j].add(x)
			Rinit[i].add(x)
			Binit[block_index(i,j)].add(x)

for i in range(0,n):
	R[i] = R[i]-Rinit[i]
	C[i] = C[i]-Cinit[i]
	B[i] = B[i]-Binit[i]

import heapq
import itertools
import copy
def priority(i,j):
	possibilities = R[i] & C[j] & B[block_index(i,j)]
	return len(possibilities)

def set_priorities():
	return [ (priority(i,j),i,j) for i,j in itertools.product(range(0,n),range(0,n)) if Solution[i][j] == 0]

current_zeros = set_priorities()
#[ (priority(i,j),i,j) for i,j in itertools.product(range(0,n),range(0,n)) if Solution[i][j] == 0]

heapq.heapify(current_zeros)

def assign_singleton(i,j):
	p = R[i] & C[j] & B[block_index(i,j)]
	print(p)
	if not p:
		return -1
	v = p.pop()
	while (v,i,j) in BadGuesses and not p:
		v = p.pop() 
	if (v,i,j) in BadGuesses:
		return -1
	Solution[i][j] = v
	R[i].remove(v)
	C[j].remove(v)
	B[block_index(i,j)].remove(v)
	return v

def update_priorities(L):
	for li in range(0,len(L)):
		_,i,j = L[li]
		L[li] = (priority(i,j),i,j)
	heapq.heapify(L)

def apply_known(current_zeros):
	while current_zeros:
		p,i,j = heapq.heappop(current_zeros)
		if p == 1:
			if assign_singleton(i,j) != -1:
				update_priorities(current_zeros)


States = []
Guesses = []

def speculate(current_zeros):
	#Save the current state, make 1 guess on lowest priority node
	p,i,j = heapq.heappop(current_zeros)
	States.append(copy.deepcopy(Solution))

	v = assign_singleton(i,j)
	Guesses.append((v,i,j))

def reset_RCB():
	for i in range(0,n):
		R[i] = set([ j+1 for j in range(0,n)] )
		C[i] = set([ j+1 for j in range(0,n)] )
		B[i] = set([ j+1 for j in range(0,n)] )
	for i,j in itertools.product(range(0,n),range(0,n)):
		x = Solution[i][j]
		if x!= 0:
			R[i].remove(x)
			C[j].remove(x)
			B[block_index(i,j)].remove(x)
	

def go_back():
	global Solution
	if len(States)==0:
		#We have reached no valid solutions
		#Puzzle is not well defined
		return []
	v,i,j = Guesses.pop()
	BadGuesses.add((v,i,j))
	Solution = States.pop()
	#Rebuild R,C,B
	reset_RCB()
	return set_priorities()

def block_indices(bi,bj):
	L = []
	for i in range(n_sub*bi,n_sub*(bi+1)):
		for j in range(n_sub*bj,n_sub*(bj+1)):
			L.append((i,j))
	return L

def is_valid_solution():
	Stgt = set([i+1 for i in range(0,n)])
	val = True
	for i in range(0,n):
		Sr = set(Solution[i])
		Sc = set([ Solution[j][i] for j in range(0,n)])
		val = val and (Sr==Stgt) and (Sc==Stgt)
		if not val:
			return val
	for i,j in itertools.product(range(0,n_sub),range(0,n_sub)):
		L = block_indices(i,j)
		Sb = set([ Solution[p][q] for p,q in L])
		val = val and (Sb==Stgt)
		if not val:
			return val
	return val

def solve(current_zeros):
	#while current_zeros:
	for i in range(0,1):
		print("---- Start ----")
		print(i)
		print(current_zeros)
		print("---- Grid ----")
		for j in range(0,n):
			print(Solution[j])
		apply_known(current_zeros)
		if is_valid_solution():
			break
		current_zeros = set_priorities()
		print("---- Applied ----")
		print(i)
		print(current_zeros)
		print("---- Grid ----")
		for j in range(0,n):
			print(Solution[j])
		#if len(current_zeros)==0:
		#	current_zeros = go_back()
		#else:
		#	speculate(current_zeros)

solve(current_zeros)

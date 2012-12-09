
import unittest
import time
import random
#Finishing the set inplementation from code retreat 08122012, Toronto.


class GameOfLifeTest(unittest.TestCase):
	
	def test_check(self):
		self.assertEquals(1,1)

	def test_twitter_feed(self):
		pass

	def test_grid_init(self):
		g = GOF([])
		self.assertEquals(g.cords,set())

	def test_single_cord_grid(self):
		g = GOF([(1,1)])
		self.assertEquals(g.cords,set([(1,1)]))

	def test_alive_neighbours(self):
		g = GOF([(1,1),(1,3),(3,3),\
				(5,5),(7,8),(12,3)])
		self.assertEquals(g.get_num_neighbours((2,2)),3)
		self.assertEquals(g.get_num_neighbours((7,8)),1)

	def test_cell_death(self):
		clist = [(3,3),(5,5)]
		g = GOF(clist)
		g.next_state()
		for c in clist:
			val = c in g.cords
			self.assertEquals(val,False)

	def test_2or3_live_(self):
		clist = [(1,1),(1,2),(1,3)]
		g = GOF(clist)
		g.next_state()
		self.assertEquals((1,1) in g.cords,False)
		self.assertEquals((1,2) in g.cords,True)
		self.assertEquals((1,3) in g.cords,False)
	
	def test_birth(self):
		g = GOF([(1,1),(1,2),(1,3)])
		#(2,2) alive and (0,2) 
		g.next_state()
		self.assertEquals((2,2) in g.cords,True)
		self.assertEquals((0,2) in g.cords,True)

	def test_run(self):
		n = 5
		n_alive = int(n*n*random.random())
		L = [ (random.randint(0,n),random.randint(0,n)) for _ in range(0,n_alive) ]
		g = GOF(L)
		while True:
			print(g)
			time.sleep(2)
			g.next_state()

class GOF(object):
	def __init__(self, cords):
		self.cords = set(cords)
		self.border = self.get_border()
	
	def __str__(self):
		L = zip(*self.cords)
		xmin,xmax,ymin,ymax = 0,0,0,0
		if L:
			xmin,xmax = min(L[0]),max(L[0])
			ymin,ymax = min(L[1]),max(L[1])
		G = [ [ 0 for _ in range(0,xmax-xmin+1)] \
				for _ in range(0,ymax-ymin+1) ] 
		for c in self.cords:
			x,y = c
			G[y-ymin][x-xmin] = 1
		out = "-----------------------\n"
		out = out + "(xmin,ymin) = "+str((xmin,ymin))+"\n"
		for r in G:
			out = out + " ".join([str(v) for v in r])+"\n"
		out = out + "(xmax,ymax) = "+str((xmax,ymax))+"\n"
		return out


	def get_border(self):
		border = set()
		for c in self.cords:
			neighbours = self.get_neighbours(c)
			dead_cells = [ nbr for nbr in neighbours \
					if nbr not in self.cords ]
			for d in dead_cells:
				border.add(d)
			#border.union(dead_cells) 
		return border

	def get_neighbours(self, cord):
		x,y = cord
		L = [ (i,j) 
			for i in range(x-1,x+2) 
				for j in range(y-1,y+2) 
					if not (i==x and j==y) ]
		return L

	def update_cell(self,cord,alive):
		n = self.get_num_neighbours(cord)
		if alive:
			return n==3 or n==4
		else:
			return n==3
		
	def next_state(self):

		new_cords = set()
		for cord in self.cords:
			if self.update_cell(cord,True):
				new_cords.add(cord)
		for cord in self.border:
			if self.update_cell(cord,False):
				new_cords.add(cord)
		self.cords = new_cords
		self.border = self.get_border()

	def get_num_neighbours(self, cord):
		#Counts itself
		x,y = cord
		acc = 0
		for i in range(x-1,x+2):
			for j in range(y-1,y+2):
				c = (i,j)
				acc = acc + (c in self.cords)
		return acc
		

if __name__ == "__main__":
	unittest.main()




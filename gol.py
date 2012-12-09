
import unittest


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
		self.assertEquals(g.get_num_neighbours((2,2)),2)
		self.assertEquals(g.get_num_neighbours((7,8)),0)

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

class GOF(object):
	def __init__(self, cords):
		self.cords = set(cords)

	def animate_dead(self, cord):
		x,y = cord

		for i in range(x-1,x+2):
			for j in range(y-1,y+2):
				c = (i,j)
				acc = acc + (c in self.cords)
				
		if acc > 3:
			return True
		else:
			return False

		
	def next_state(self):

		new_cords = set()
		for cord in self.cords:
			# < than 2 dies
			n = self.get_num_neighbours(cord)
			if self.get_num_neighbours(cord) < 2:
				pass
			elif n>4:
				pass
			else:
				new_cords.add(cord)	

		self.cords = new_cords



	def get_num_neighbours(self, cord):
		x,y = cord
		acc = 0
		for i in range(x-1,x+2):
			for j in range(y-1,y+2):
				c = (i,j)
				acc = acc + (c in self.cords)

		return acc-1
		

if __name__ == "__main__":
	unittest.main()



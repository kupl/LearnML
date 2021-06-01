let rec sigma (test : int->int) (x : int) (y : int) : int  = 
				if x <= y then test x + (sigma test (x+1) y)
				else 0

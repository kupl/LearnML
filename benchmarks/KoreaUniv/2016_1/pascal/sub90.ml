let rec pascal : int*int -> int
	=fun(a,b)->
		if b=0 || a = b then 1
		else pascal(a-1,b-1) + pascal (a-1,b);;

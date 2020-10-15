(*2009-11718 박준상 1-1*)

let rec sigma f a b =
	if a>b then 0
		else if a=b then (f a)
		else (f b)+ sigma f a (b-1)


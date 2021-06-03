let rec sigma test a b =
	if a = b then (test a)
	else (test b) + (sigma test a (b-1));;


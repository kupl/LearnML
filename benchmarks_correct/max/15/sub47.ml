let rec max : int list -> int
=fun l -> match l with
| [] -> 0
| [x] -> x
| h :: t -> let mx = max t in
	if h > mx then h
	else mx
	 
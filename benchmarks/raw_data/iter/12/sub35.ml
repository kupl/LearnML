let rec iter (a,b) =
	match (a,b) with
	|(0,_) -> fun x -> x
	|_ -> fun x -> iter (a-1,b) (b x)

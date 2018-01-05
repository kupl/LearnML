let rec sumprod (a,b,c) =
	let rec calc (a,b,c) =
		match (a,b,c) with
		|(a,b,1) -> a(b,1)
		|_ -> a(b,c) *. calc(a,b,c-1)
		in
	match (a,b,c) with
	|(a,1,c) -> calc(a,1,c)
	|_ -> calc (a,b,c) +. sumprod (a,b-1,c)

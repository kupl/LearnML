let sumprod (mat,n,k) =
	let rec sphelper x =
		let rec sphelper2 y =
			match y-k with
				| 0 -> mat (x,y)
				| _ -> (mat (x,y)) *. (sphelper2 (y+1))
			in
		match x-n with
			| 0 -> sphelper2 1
			| _ -> (sphelper2 1) +. (sphelper (x+1))
		in
	sphelper 1
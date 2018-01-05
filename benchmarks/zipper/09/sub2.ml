let rec zipper ((a:int list), (b:int list)) =
	match a with
		[] ->
		b
		| x::xs ->
		match b with
				y::ys -> (x::y::[])@zipper(xs, ys)
				|[] -> x::xs
			

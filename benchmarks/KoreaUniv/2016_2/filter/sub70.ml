let filter (fn : 'a->bool)(xs: 'a list): 'a list =
	let rec aux xs acc =
		match xs with 
		| []->acc
		| y::ys-> if(fn y) then (aux ys (acc@[y])) then (aux ys acc) in
	aux xs []	

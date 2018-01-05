
let rec iter n f x =
	if n=0 then x
	else if n=1 then f x
	else let n = n -1 in 
		let fx = f x in iter n f fx

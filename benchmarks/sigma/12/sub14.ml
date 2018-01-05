let res = []

let rec func(a, b, tmp) =
	if  a=b then res :: tmp(a)
	else if a<b then  res :: tmp(b)
		func(a, (b-1), tmp)


let rec sum l =
	match l with
	| [] -> 0
	| h::t -> h + sum t


let sigma(a, b, tmp) =
	func(a, b, tmp)
	sum res



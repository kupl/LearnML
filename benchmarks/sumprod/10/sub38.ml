exception Error of string

let rec sumprod matrix a b =
	let rec sub_sump matrix a b =
		let i = 1 in
		if (b < 0) then raise (Error "error")
		else if (b = i) then (matrix a b) 
		else (matrix a b) *. (sub_sump matrix a (b-1)) in
	let i = 1 in
	if (a < 0) then raise (Error "error")
	else if (a = i) then (sub_sump matrix a b)
	else (sub_sump matrix a b) +. (sumprod matrix (a-1) b);;


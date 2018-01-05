(* HW 1-1 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

let sigma (cur, last, f) =
	let rec inner (cur, last, f) = 
		if cur > last then
			0
		else
			(f cur) + (inner ((cur+1), last, f))
	in

	if cur > last then
		raise (Invalid_argument "sigma")
	else
		inner (cur, last, f)
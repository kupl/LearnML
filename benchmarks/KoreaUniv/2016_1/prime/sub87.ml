let rec prime : int -> bool
= fun n ->

	let rec check a =
	if a = 1 then true
	else if n mod a = 0 then false
	else true && check (a-1) in

	if n = 1 then false
	else if n = 2 then true
	else check (n-1)

(* Ex 2. Sigma *)
let rec sigma (a, b, f) =
	if a > b then 0
	else if a == b then (f a)
	else (f a) + (sigma ((a+1), b, f))
(*
let _ =
	let msg = string_of_int (sigma (1, 3, (fun x -> 2 * x))) in
	print_endline msg

let _ =
	let msg = string_of_int (sigma (1, 3, (fun x -> x * x))) in
	print_endline msg
*)
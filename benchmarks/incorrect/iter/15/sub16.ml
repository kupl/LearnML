(* Ex 3. Iterator *)
let rec iter (n, f) x =
	if n == 1 then (f x)
	else iter (n-1, f) (f x)

(*
let _ =
	let msg = string_of_int (iter (6, (fun x -> 2+x)) 0) in
	print_endline msg
*)

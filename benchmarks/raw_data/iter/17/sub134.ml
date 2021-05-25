(*Computer Engineering 2015-12683 Kim Jaein Exercise 1-3*)
let rec iter (n, f) =
	if n <= 0 then (fun x -> x)
	else if n == 1 then (fun x -> f x)
	else (fun x -> f ((iter ((n-1), f)) x))
(*
let twice x = x * x

let () = print_endline (string_of_int (iter (2, twice) 3))
*)

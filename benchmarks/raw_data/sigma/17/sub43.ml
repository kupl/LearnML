let sigma : int * int * (int -> int) -> int = fun (a,b,f) ->
	let rec help = fun (a, b, f, r) ->
		if a=b then (f a) +r
		else if a>b then 0
		else help(a+1,b,f,r+(f a))
	in
	help(a,b,f,0)
(*
let uni : int -> int = fun x -> x

let _ = print_endline(string_of_int(sigma(9, 10, uni)))

let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (385 = sigma (1, 10, (fun x -> x * x))); 
print_bool (0 = sigma (3, 1, fun x -> x * x)); 
print_bool (27 = sigma(3, 3, fun x -> x * x * x)); 
print_bool (385 = sigma(-10, -1, fun x -> x * x)) 
*)

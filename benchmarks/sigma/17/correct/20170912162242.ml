let rec sigma ((a : int), (b : int), (f : int -> int)) : int = 
	if (a > b) then 0
	else f (a) + sigma(a + 1, b, f)

let a = 0
let b = 10
let f = fun x -> x
let expt = 55

open Printf
let _ = print_endline(string_of_int (sigma(a, b, f)))
let _ = print_endline(string_of_bool (expt = sigma(a, b, f)))


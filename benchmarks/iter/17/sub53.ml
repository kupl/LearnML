let rec iter (n, f) x = 
	if (n <= 0) then x
	else iter (n-1, f) (f x)

let a = iter(10, function x -> 2+x) 0
let exp = 20

let _ = print_endline(string_of_int a)
let _ = print_endline(string_of_bool (a = exp))

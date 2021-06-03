let rec sigma f a b =
	if a > b then 0
	else sigma f (a+1) b + f (a)
(*
let _ =
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (385 = sigma (1, 10, (fun x -> x * x))); 
print_bool (0 = sigma (3, 1, fun x -> x * x)); 
print_bool (27 = sigma(3, 3, fun x -> x * x * x)); 
print_bool (385 = sigma(-10, -1, fun x -> x * x)) ;

print_bool (6 = sigma(0, 3, fun x -> x)) ;
print_bool (3 = sigma(3, 3, fun x -> x)) ;
print_bool (294 = sigma(7, 10, fun x -> x * x)) ;
print_bool (0 = sigma(11, 10, fun x -> x * x)) ;
*)

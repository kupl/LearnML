let rec sumprod (matrix, n, k) =
	let rec pie (matrix, n, k) =
	if (k >= 1)
	then
		if (k = 1)
		then (matrix (n, k))
		else (matrix (n, k) *. (pie (matrix, n, k - 1)))
	else 0.0 in
	
	if (n >= 1)
	then
		if (n = 1)
		then (pie (matrix, n, k))
		else (pie (matrix, n, k)) +. (sumprod (matrix, n - 1, k))
	else 0.0

(* TEST SET*)
(*
let _ =
    print_string "sumprod Test Set\n";
    print_float (sumprod ((fun (x,y) -> float_of_int (x+y)), 1, 1));
    print_string "\n";
    print_float (sumprod ((fun (x,y) -> float_of_int (x)), 3, 5));
    print_string "\n";
    print_float (sumprod ((fun (x,y) -> float_of_int (x+y)), 3, 5));
    print_string "\n";
    print_float (sumprod ((fun (x,y) -> float_of_int (2*x-y)), 2, 3));
    print_string "\n";
    print_float (sumprod ((fun (x,y) -> float_of_int (2*y-3*x)), 2, 3));
    print_string "\n\n"
*)
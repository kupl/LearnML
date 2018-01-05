let rec iter (n, func) init =
	if (n >= 1)
	then
		if (n = 1)
		then (func init)
		else (iter (n - 1, func) (func init))
	else
		(func init)

(* TEST SET *)
(*
let _ =
    print_string "iter Test Set\n";
    print_int (iter (5, (function x -> 2 + x)) 0);
    print_char '\n';
    print_int (iter (0, (function x -> x * x * x)) 0);
    print_char '\n';
    print_int (iter (3, (function x -> x * x)) 5);
    print_char '\n';
    print_int (iter (12345, (function x -> x )) (-200));
    print_char '\n';
    print_int (iter (999, (function x -> (if x = 2 then 1 else 2))) 2);
    print_string "\n\n"
*)
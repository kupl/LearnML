let rec sigma func lower upper =
	if (lower <= upper)
	then
		if (lower = upper)
		then (func lower)
		else (func lower) + (sigma func (lower+1) upper)
	else 0
	
(* TEST SET *)
(*
let _ =
	print_string "sigma Test Set\n";
	print_int (sigma (1, 1, (function x -> x)));
	print_char '\n';
	print_int (sigma (1, 10, (function x -> x)));
	print_char '\n';
	print_int (sigma (-50, 50, (function x -> x * x)));
	print_char '\n';
	print_int (sigma (2, 10, (function x -> 10)));
	print_string "\n\n"
*)
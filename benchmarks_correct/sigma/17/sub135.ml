(*Computer Engineering 2015-12683 Kim Jaein Exercise 1-2*)
(*let f n = n*n;;*)

let rec sigma f a b =
	if b < a then 0
	else begin
		f a + sigma f (a+1) b
	end
(*
let () = print_endline (string_of_int (sigma (1, 3, f)));;
*)

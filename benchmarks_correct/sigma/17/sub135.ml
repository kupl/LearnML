(*Computer Engineering 2015-12683 Kim Jaein Exercise 1-2*)
(*let f n = n*n;;*)

let rec sigma ((a:int), (b:int), (f:int -> int)) =
	if b < a then 0
	else begin
		f a + sigma ((a+1), b, f)
	end
(*
let () = print_endline (string_of_int (sigma (1, 3, f)));;
*)

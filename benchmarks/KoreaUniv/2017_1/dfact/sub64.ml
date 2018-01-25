(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
	if a >= b then (f a)
	else (f a) * (product f (a+1) b)
;;
(*
let test4 = product (fun x -> x) in
print_endline (string_of_int (test4 1 5));;
let test4_2 = product (fun x -> x + 1) in
print_endline (string_of_int (test4_2 2 4));;
*)

(* problem 5*)

let dfact : int -> int
= fun n -> 
	if (n mod 2) == 0 then (* n is even, ignore the odd x values *)
		product (fun x -> if x mod 2 == 1 then 1 else x) 1 n
	else (* n is odd, ignore the even x values *)
		product (fun x -> if x mod 2 == 0 then 1 else x) 1 n

;;
(*
print_endline (string_of_int (dfact 7));;
print_endline (string_of_int (dfact 8));;
*)
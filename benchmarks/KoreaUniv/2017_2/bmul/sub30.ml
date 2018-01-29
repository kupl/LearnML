(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> 
let rec pow : int -> int
= fun p  -> if p = 0 then 1 else 2*pow(p - 1)
in let b_digit : digit -> int
= fun bdigit -> 
begin
	match bdigit with
	| ZERO -> 0
	| ONE -> 1
end
in let rec conv_bin : bin -> int
= fun lst -> (
	match lst with
	| [] -> raise (Failure "Empty Binary List")
	| hd::[] -> b_digit(hd)
	| hd::tl -> b_digit(hd)*(pow ((List.length lst)-1)) + conv_bin(tl))
in let calc_mul : int -> int -> int
= fun a b -> a*b
in let rec conv_int : int -> bin
= fun x -> if x = 0 then [ZERO]
					else if x = 1 then [ONE]
					else List.append (conv_int(x/2)) (conv_int(x mod 2))
in conv_int((calc_mul (conv_bin(b1))  (conv_bin(b2))));;


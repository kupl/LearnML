(* complete *)
type lambda = V of var
	|P of var * lambda
	|C of lambda * lambda
and var = string

let check met =
	let rec search n l = match l with
		[] -> false
		|h::t -> if h = n then true
				else search n t
	in
	let rec check2 m lst = match m with
		V n -> (search n lst)
		|P (a,b) -> check2 b (a::lst)
		|C (a,b) -> (check2 a lst)&&(check2 b lst)
	in
	check2 met []
;;

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
	let rec check m lst = match m with
		V n -> (search n lst)
		|P (a,b) -> check b (a::lst)
		|C (a,b) -> (check a lst)&&(check b lst)
	in
	check met []
;;

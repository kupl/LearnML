type lambda = V of var
         | P of var * lambda
         | C of lambda * lambda
and var = string

let rec vpc_eval lambda1 ar_env =
		match lambda1 with
		V a ->
				let rec ck l1 st =
					match l1 with
					[] -> false |
					h::t -> if h=st then true else (ck t st) in
				if (ck ar_env a) then true else false |
		P (v, e1) -> if (vpc_eval e1 (ar_env@[v])) then true else false |
		C (e1, e2) -> if (vpc_eval e1 ar_env && vpc_eval e2 ar_env) then true else false;;

let check : lambda -> bool
=fun e ->
			vpc_eval e [];;

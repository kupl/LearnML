type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let rec vpc_eval exp1 ar_env =
		match exp1 with
		V a ->
				let rec ck l1 st =
					match l1 with
					[] -> false |
					h::t -> if h=st then true else (ck t st) in
				if (ck ar_env a) then true else false |
		P (v, e1) -> if (vpc_eval e1 (ar_env@[v])) then true else false |
		C (e1, e2) -> if (vpc_eval e1 ar_env && vpc_eval e2 ar_env) then true else false;;

let check : exp -> bool
=fun e ->
			vpc_eval e [];;

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string



let rec check : lambda -> bool
= fun lam ->
let rec eval : lambda -> var list -> var list
 = fun lam ll->
		match lam with 
		| V v -> (match ll with
						 | [] -> ("false")::ll
						 | hd::tl -> if hd = v then ll else hd::(eval (V v) tl))
		| P (v,l) -> let ll2 = v::ll in eval l ll2
		| C (l1,l2) ->  eval l2 (eval l1 ll)
in let rec compare : var list -> bool
 = fun ll -> 
		match ll with
		| [] -> true
		| hd::tl -> if hd = "false" then false else compare tl
in compare (eval lam [])


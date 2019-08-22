(*
 CSE 2012-11226 Kwak Jin Han
 exercise 4
 *)

type lambda = V of var
					 | P of var * lambda
					 | C of lambda * lambda
	and var = string

(*
(* true *)
let a = P ("a", V "a")
let b = P ("a", P ("a", V "a"))
let c = P ("a", P ("b", C (V "a", V "b")))
let d = P ("a", C (V "a", P ("b", V "a")))
(* false *)
let e = P ("a", V "b")
let f = P ("a", C (V "a", P ("b", V "c")))
let g = P ("a", P ("b", C (V "a", V "c")))
*)

(* check : lambda -> bool *)
let (* rec *) check m =
	let rec main (m, saveStation) = 
		match m with
		| V pvar -> 
				if List.mem pvar saveStation = true then true
				else false

		| P (pvar, plambda) -> 
				main (plambda, pvar::saveStation)

		| C (leftlambda, rightlambda) ->
				main (leftlambda, saveStation) && main (rightlambda, saveStation) in
	main(m, [])

type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> check_aux lam []

and check_aux : lambda -> string list -> bool
= fun lam li ->
  match lam with
    | V x -> (is_include x li)
    | P (x,lmd) -> check_aux lmd (x::li) 
    | C(lmd1, lmd2) -> (check_aux lmd1 li) && (check_aux lmd2 li) 
      
and is_include: 'a -> 'a list -> bool
= fun a li ->
	match li with
	|[]-> false
	|h::t -> if a = h then true else (is_include a t) ;; 
	

check (P ("a", C (V "a", P ("b", V "a"))));;


(*
P ("a", V "a")
P ("a", P ("a", V "a"))
P ("a", P ("b", C (V "a", V "b")))
P ("a", C (V "a", P ("b", V "a")))*)

(*
P ("a", V "b")
P ("a", C (V "a", P ("b", V "c")))
P ("a", P ("b", C (V "a", V "c")))*)

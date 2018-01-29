(* ------------------problem3------------------ *)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec sat : formula -> bool
= fun f -> 
	let test fm =
		match fm with
		| Var(s) -> true
		| _ -> false in
	match f with
	| True -> true
	| False -> false
	| Var(s) -> true
	| Neg(f1) -> if not (sat f1) then true else (test f1)
	| And(f1, f2) -> if ((sat f1) && (sat f2)) then true else false
	| Or(f1, f2) -> if((sat f1) || (sat f2)) then true else false
	| Imply(f1, f2) -> if (not (sat f1)) || (test f1) then true else (sat f2)  (*var수정*)
	| Iff(f1, f2) -> (sat (And( Imply(f1, f2), Imply(f2, f1))));;
(* -------------------------------------------- *)

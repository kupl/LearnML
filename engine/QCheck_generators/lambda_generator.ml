(* ./main.native -qcheck -solution ../benchmarks2/KoreaUniv_wellformedness/sol.ml -entry check -generator lambda -submission ../benchmarks2/KoreaUniv_wellformedness/15/sub14.ml *)

type t = exp
and exp =
  | V of var
  | P of var * exp
  | C of exp * exp
and var = int

let to_string : t -> string
= fun m -> 
	let rec string_of_numeral n =
		let n = (n mod 26) + 97 in
		"\"" ^ Char.escaped (Char.chr n) ^ "\""
	in
	let rec string_of_exp m =
		match m with
		| V n -> "V (" ^ string_of_numeral n ^ ")" 
  		| P (n, m) -> "P (" ^ string_of_numeral n ^ "," ^ string_of_exp m ^ ")" 
  		| C (m1, m2) -> "C (" ^ string_of_exp m1 ^ "," ^ string_of_exp m2 ^ ")"
  	in
	string_of_exp m

let shrink : t -> t QCheck.Iter.t
= fun m -> 
	let open QCheck.Iter in
	let rec shrink_exp m =
		match m with
		| V n -> map (fun n' -> V n') (QCheck.Shrink.int n)
  		| P (n, m) -> 
  			map (fun n' -> P (n', m)) (QCheck.Shrink.int n)
  			<+> map (fun m' -> P (n, m')) (shrink_exp m)
  		| C (m1, m2) -> 
  			of_list [m1; m2]
  			<+> map (fun m1' -> C (m1', m2)) (shrink_exp m1)
  			<+> map (fun m2' -> C (m1, m2')) (shrink_exp m2)
	in
	shrink_exp m

let gen : t QCheck.Gen.t 
= 
	let open QCheck.Gen in
	let gen_exp = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> map (fun n -> V n) (nat)
			| _ ->
				frequency [
				 1, map2 (fun n m -> P (n, m)) (nat) (recgen (n/2));
				 1, map2 (fun m1 m2 -> C (m1, m2)) (recgen (n/2)) (recgen (n/2));
				])
		)
	in
	gen_exp


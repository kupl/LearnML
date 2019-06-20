(* ./main.native -qcheck -solution ../benchmarks2/checkMetro/sol.ml -entry checkMetro -generator checkMetro -submission ../benchmarks2/checkMetro/09/sub15.ml *)

type t = metro
and metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = int

let to_string : t -> string
= fun m -> 
	let rec string_of_numeral n =
		let n = (n mod 26) + 97 in
		"\"" ^ Char.escaped (Char.chr n) ^ "\""
	in
	let rec string_of_metro m =
		match m with
		| STATION n -> "STATION (" ^ string_of_numeral n ^ ")" 
  		| AREA (n, m) -> "AREA (" ^ string_of_numeral n ^ "," ^ string_of_metro m ^ ")" 
  		| CONNECT (m1, m2) -> "CONNECT (" ^ string_of_metro m1 ^ "," ^ string_of_metro m2 ^ ")"
  	in
	string_of_metro m

let shrink : t -> t QCheck.Iter.t
= fun m -> 
	let open QCheck.Iter in
	let rec shrink_metro m =
		match m with
		| STATION n -> map (fun n' -> STATION n') (QCheck.Shrink.int n)
  		| AREA (n, m) -> 
  			map (fun n' -> AREA (n', m)) (QCheck.Shrink.int n)
  			<+> map (fun m' -> AREA (n, m')) (shrink_metro m)
  		| CONNECT (m1, m2) -> 
  			of_list [m1; m2]
  			<+> map (fun m1' -> CONNECT (m1', m2)) (shrink_metro m1)
  			<+> map (fun m2' -> CONNECT (m1, m2')) (shrink_metro m2)
	in
	shrink_metro m

let gen : t QCheck.Gen.t 
= 
	let open QCheck.Gen in
	let gen_metro = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> map (fun n -> STATION n) (nat)
			| _ -> 
				frequency [
				 1, map2 (fun n m -> AREA (n, m)) (nat) (recgen (n/2));
				 1, map2 (fun m1 m2 -> CONNECT (m1, m2)) (recgen (n/2)) (recgen (n/2));
				])
		)
	in
	gen_metro
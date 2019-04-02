(* ./main.native -qcheck -solution ../benchmarks2/formula/sol.ml -entry formula -generator formula1 -submission ../benchmarks2/formula/09/sub15.ml *)

type t = formula
and formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let to_string : t -> string
= fun f -> 
	let rec string_of_expr e =
		match e with
		| NUM n -> "NUM (" ^ string_of_int n ^ ")"
  		| PLUS (e1, e2) -> "PLUS (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
 	 	| MINUS (e1, e2) -> "MINUS (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
 	in
	let rec string_of_formula f =
		match f with
		| TRUE -> "TRUE"
		| FALSE -> "FALSE"
		| NOT f -> "NOT (" ^ string_of_formula f ^ ")"
		| ANDALSO (f1, f2) -> "ANDALSO (" ^ string_of_formula f1 ^ "," ^ string_of_formula f2 ^ ")"
		| ORELSE (f1, f2) -> "ORELSE (" ^ string_of_formula f1 ^ "," ^ string_of_formula f2 ^ ")"
		| IMPLY (f1, f2) -> "IMPLY (" ^ string_of_formula f1 ^ "," ^ string_of_formula f2 ^ ")"
		| LESS (e1, e2) -> "LESS (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
	in
	string_of_formula f

let shrink : t -> t QCheck.Iter.t
= fun f -> 
	let open QCheck.Iter in
	let rec shrink_expr e =
		match e with
		| NUM n -> map (fun n' -> NUM n') (QCheck.Shrink.int n)
		| PLUS (e1, e2) -> 
  			(of_list [e1; e2])
			<+> (map (fun e1' -> PLUS (e1', e2)) (shrink_expr e1))
			<+> (map (fun e2' -> PLUS (e1, e2')) (shrink_expr e2))
 	 	| MINUS (e1, e2) -> 
  			(of_list [e1; e2])
			<+> (map (fun e1' -> MINUS (e1', e2)) (shrink_expr e1))
			<+> (map (fun e2' -> MINUS (e1, e2')) (shrink_expr e2))
 	in
	let rec shrink_formula f =
		match f with
		| TRUE | FALSE -> empty
		| NOT f -> map (fun f' -> NOT f') (shrink_formula f)
		| ANDALSO (f1, f2) -> 
			(of_list [f1; f2])
			<+> (map (fun f1' -> ANDALSO (f1', f2)) (shrink_formula f1))
			<+> (map (fun f2' -> ANDALSO (f1, f2')) (shrink_formula f2))
		| ORELSE (f1, f2) -> 
			(of_list [f1; f2])
			<+> (map (fun f1' -> ORELSE (f1', f2)) (shrink_formula f1))
			<+> (map (fun f2' -> ORELSE (f1, f2')) (shrink_formula f2))
		| IMPLY (f1, f2) -> 
			(of_list [f1; f2])
			<+> (map (fun f1' -> IMPLY (f1', f2)) (shrink_formula f1))
			<+> (map (fun f2' -> IMPLY (f1, f2')) (shrink_formula f2))
		| LESS (e1, e2) -> 
  			(map (fun e1' -> LESS (e1', e2)) (shrink_expr e1))
			<+> (map (fun e2' -> LESS (e1, e2')) (shrink_expr e2))
	in
	shrink_formula f

let gen : t QCheck.Gen.t 
= 
	let open QCheck.Gen in
	let gen_expr = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> map (fun n -> NUM n) small_int
			| _ -> 
				frequency [
				 1, map (fun n -> NUM n) small_int; 
				 2, map2 (fun e1 e2 -> PLUS (e1, e2)) (recgen (n/2)) (recgen (n/2));
				 2, map2 (fun e1 e2 -> MINUS (e1, e2)) (recgen (n/2)) (recgen (n/2));
				])
		)
	in
	let gen_formula = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> oneof [return TRUE; return FALSE]
			| _ -> 
				frequency [
				 1, oneof [return TRUE; return FALSE]; 
				 2, map (fun f -> NOT f) (recgen (n/2));
				 2, map2 (fun f1 f2 -> ANDALSO (f1, f2)) (recgen (n/2)) (recgen (n/2));
				 2, map2 (fun f1 f2 -> ORELSE (f1, f2)) (recgen (n/2)) (recgen (n/2));
				 2, map2 (fun f1 f2 -> IMPLY (f1, f2)) (recgen (n/2)) (recgen (n/2));
				 2, map2 (fun e1 e2 -> LESS (e1, e2)) gen_expr gen_expr;
				])
		)
	in
	gen_formula
type t = formula
and formula =
   | True
   | False
   | Not of formula
   | AndAlso of formula * formula
   | OrElse of formula * formula
   | Imply of formula * formula
   | Equal of exp * exp
and exp =
   | Num of int
   | Plus of exp * exp
   | Minus of exp * exp

let to_string : t -> string
= fun f -> 
	let rec string_of_expr e =
		match e with
		| Num n -> "Num (" ^ string_of_int n ^ ")"
  		| Plus (e1, e2) -> "Plus (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
 	 	| Minus (e1, e2) -> "Minus (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
 	in
	let rec string_of_formula f =
		match f with
		| True -> "True"
		| False -> "False"
		| Not f -> "Not (" ^ string_of_formula f ^ ")"
		| AndAlso (f1, f2) -> "AndAlso (" ^ string_of_formula f1 ^ "," ^ string_of_formula f2 ^ ")"
		| OrElse (f1, f2) -> "OrElse (" ^ string_of_formula f1 ^ "," ^ string_of_formula f2 ^ ")"
		| Imply (f1, f2) -> "Imply (" ^ string_of_formula f1 ^ "," ^ string_of_formula f2 ^ ")"
		| Equal (e1, e2) -> "Equal (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
	in
	string_of_formula f

let shrink : t -> t QCheck.Iter.t
= fun f -> 
	let open QCheck.Iter in
	let rec shrink_expr e =
		match e with
		| Num n -> map (fun n' -> Num n') (QCheck.Shrink.int n)
		| Plus (e1, e2) -> 
  			(of_list [e1; e2])
			<+> (map (fun e1' -> Plus (e1', e2)) (shrink_expr e1))
			<+> (map (fun e2' -> Plus (e1, e2')) (shrink_expr e2))
 	 	| Minus (e1, e2) -> 
  			(of_list [e1; e2])
			<+> (map (fun e1' -> Minus (e1', e2)) (shrink_expr e1))
			<+> (map (fun e2' -> Minus (e1, e2')) (shrink_expr e2))
 	in
	let rec shrink_formula f =
		match f with
		| True | False -> empty
		| Not f -> map (fun f' -> Not f') (shrink_formula f)
		| AndAlso (f1, f2) -> 
			(of_list [f1; f2])
			<+> (map (fun f1' -> AndAlso (f1', f2)) (shrink_formula f1))
			<+> (map (fun f2' -> AndAlso (f1, f2')) (shrink_formula f2))
		| OrElse (f1, f2) -> 
			(of_list [f1; f2])
			<+> (map (fun f1' -> OrElse (f1', f2)) (shrink_formula f1))
			<+> (map (fun f2' -> OrElse (f1, f2')) (shrink_formula f2))
		| Imply (f1, f2) -> 
			(of_list [f1; f2])
			<+> (map (fun f1' -> Imply (f1', f2)) (shrink_formula f1))
			<+> (map (fun f2' -> Imply (f1, f2')) (shrink_formula f2))
		| Equal (e1, e2) -> 
  			(map (fun e1' -> Equal (e1', e2)) (shrink_expr e1))
			<+> (map (fun e2' -> Equal (e1, e2')) (shrink_expr e2))
	in
	shrink_formula f

let gen : t QCheck.Gen.t 
= 
	let open QCheck.Gen in
	let gen_expr = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> map (fun n -> Num n) small_int
			| _ -> 
				frequency [
				 2, map (fun n -> Num n) small_int; 
				 1, map2 (fun e1 e2 -> Plus (e1, e2)) (recgen (n/2)) (recgen (n/2));
				 1, map2 (fun e1 e2 -> Minus (e1, e2)) (recgen (n/2)) (recgen (n/2));
				])
		)
	in
	let gen_formula = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> oneof [return True; return False]
			| _ -> 
				frequency [
				 1, oneof [return True; return False]; 
				 2, map (fun f -> Not f) (recgen (n/2));
				 2, map2 (fun f1 f2 -> AndAlso (f1, f2)) (recgen (n/2)) (recgen (n/2));
				 2, map2 (fun f1 f2 -> OrElse (f1, f2)) (recgen (n/2)) (recgen (n/2));
				 2, map2 (fun f1 f2 -> Imply (f1, f2)) (recgen (n/2)) (recgen (n/2));
				 2, map2 (fun e1 e2 -> Equal (e1, e2)) gen_expr gen_expr;
				])
		)
	in
	gen_formula
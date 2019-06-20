type t = (crazy2 * crazy2)
and crazy2 =
	| NIL
	| ZERO of crazy2
	| ONE of crazy2
	| MONE of crazy2

let to_string : t -> string
= fun (c1, c2) -> 
	let rec string_of_crazy2 c =
		match c with
		| NIL -> "NIL"
		| ZERO c' -> "ZERO (" ^ string_of_crazy2 c' ^ ")"
		| ONE c' -> "ONE (" ^ string_of_crazy2 c' ^ ")"
		| MONE c' -> "MONE (" ^ string_of_crazy2 c' ^ ")"
	in
	"(" ^ string_of_crazy2 c1 ^ ", " ^ string_of_crazy2 c2 ^ ")"

let shrink : t -> t QCheck.Iter.t
= fun (c1, c2) -> 
	let open QCheck.Iter in
	let rec shrink_crazy2 c =
		match c with
		| NIL -> empty
		| ZERO c -> (map (fun c' -> ZERO c') (shrink_crazy2 c))
		| ONE c -> (map (fun c' -> ONE c') (shrink_crazy2 c))
		| MONE c -> (map (fun c' -> MONE c') (shrink_crazy2 c))
	in
	pair (shrink_crazy2 c1) (shrink_crazy2 c2)

let gen : t QCheck.Gen.t 
= 
	let open QCheck.Gen in
	let gen_crazy2 = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> return NIL
			| _ -> 
				frequency [
				 1, return NIL; 
				 2, map (fun c -> ZERO c) (recgen (n/2));
				 2, map (fun c -> ONE c) (recgen (n/2));
				 2, map (fun c -> MONE c) (recgen (n/2))
				])
		)
	in
	pair gen_crazy2 gen_crazy2
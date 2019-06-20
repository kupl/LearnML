(* ./main.native -qcheck -solution ../benchmarks2/diff/sol.ml -entry gradign -grading ../benchmarks2/diff/grading.ml -generator diff1 -submission ../benchmarks2/diff/12/sub1.ml *)

type t = ((aexp * var) * env)
and aexp =
	| Const of int
	| Var of var
	| Power of var * int
	| Times of aexp list
	| Sum of aexp list
and env = (var * int) list
and var = int

let to_string : t -> string
= fun ((aexp, x), env) -> 
	let rec string_of_numeral n =
		let n = (n mod 26) + 97 in
		"\"" ^ Char.escaped (Char.chr n) ^ "\""
	in
	let rec string_of_aexp aexp =
		match aexp with
		| Const n -> "Const (" ^ string_of_int n ^ ")"
		| Var x -> "Var (" ^ string_of_numeral x ^ ")"
		| Power (x, n) -> "Power (" ^ string_of_numeral x ^ "," ^ string_of_int n ^ ")"
		| Times aexps -> "Times [" ^ (List.fold_left (fun acc aexp -> string_of_aexp aexp ^ ";" ^ acc) "" aexps) ^ "]"
		| Sum aexps  -> "Sum [" ^ (List.fold_left (fun acc aexp -> string_of_aexp aexp ^ ";" ^ acc) "" aexps) ^ "]"
	in
	let rec string_of_env env =
		"[" ^ (List.fold_left (fun acc (x, n) -> "(" ^ string_of_numeral x ^ "," ^ string_of_int n ^ ");" ^ acc) "" env)^ "]"
	in
	"(" ^ string_of_aexp aexp ^ "," ^ string_of_numeral x ^ ")" ^ ";" ^ string_of_env env

let shrink : t -> t QCheck.Iter.t 
= fun ((aexp, x), env) -> 
  	let open QCheck.Iter in
  	let rec shrink_aexp aexp =
		match aexp with
		| Const n -> map (fun n' -> Const n') (QCheck.Shrink.int n)
		| Var x -> map (fun x' -> Var x') (QCheck.Shrink.int x)
		| Power (x, n) -> 
			map (fun x' -> Power (x', n)) (QCheck.Shrink.int x)
			<+> map (fun n' -> Power (x, n')) (QCheck.Shrink.int n)
		| Times aexps -> map (fun aexps' -> Times aexps') (QCheck.Shrink.list ~shrink:shrink_aexp aexps)
		| Sum aexps  -> map (fun aexps' -> Sum aexps') (QCheck.Shrink.list ~shrink:shrink_aexp aexps)
	in
  	let rec shrink_env env =
  		QCheck.Shrink.list ~shrink:(QCheck.Shrink.pair (QCheck.Shrink.int) (QCheck.Shrink.int)) env
	in
	pair (pair (shrink_aexp aexp) (QCheck.Shrink.int x)) (shrink_env env)

let gen : t QCheck.Gen.t 
= 
	let open QCheck.Gen in
	let gen_aexp = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> oneof [
				map (fun n -> Const n) small_int; 
				map (fun x -> Var n) nat;
				map2 (fun x n -> Power (x, n)) nat small_int
				]
			| _ -> 
				frequency [
				 1, map (fun n -> Const n) small_int; 
				 1, map (fun x -> Var n) nat;
				 1, map2 (fun x n -> Power (x, n)) nat small_int;
				 1, map (fun aexps -> Times aexps) (list_size (int_range 0 5) (recgen (n/4)));
				 1, map (fun aexps -> Sum aexps) (list_size (int_range 0 5) (recgen (n/4)));
				])
		)
	in
	let gen_env = list_repeat 5 (pair nat small_int) in
	pair (pair gen_aexp nat) gen_env
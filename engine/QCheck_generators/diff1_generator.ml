(* ./main.native -qcheck -solution ../benchmarks2/diff/sol.ml -entry gradign -grading ../benchmarks2/diff/grading.ml -generator diff1 -submission ../benchmarks2/diff/12/sub1.ml *)

type t = ((ae * var) * env)
and ae =
	| CONST of int
	| VAR of var
	| POWER of var * int
	| TIMES of ae list
	| SUM of ae list
and env = (var * int) list
and var = int

let to_string : t -> string
= fun ((ae, x), env) -> 
	let rec string_of_numeral n =
		let n = (n mod 26) + 97 in
		"\"" ^ Char.escaped (Char.chr n) ^ "\""
	in
	let rec string_of_ae ae =
		match ae with
		| CONST n -> "CONST (" ^ string_of_int n ^ ")"
		| VAR x -> "VAR (" ^ string_of_numeral x ^ ")"
		| POWER (x, n) -> "POWER (" ^ string_of_numeral x ^ "," ^ string_of_int n ^ ")"
		| TIMES aes -> "TIMES [" ^ (List.fold_left (fun acc ae -> string_of_ae ae ^ ";" ^ acc) "" aes) ^ "]"
		| SUM aes  -> "SUM [" ^ (List.fold_left (fun acc ae -> string_of_ae ae ^ ";" ^ acc) "" aes) ^ "]"
	in
	let rec string_of_env env =
		"[" ^ (List.fold_left (fun acc (x, n) -> "(" ^ string_of_numeral x ^ "," ^ string_of_int n ^ ");" ^ acc) "" env)^ "]"
	in
	"(" ^ string_of_ae ae ^ "," ^ string_of_numeral x ^ ")" ^ ";" ^ string_of_env env

let shrink : t -> t QCheck.Iter.t 
= fun ((ae, x), env) -> 
  	let open QCheck.Iter in
  	let rec shrink_ae ae =
		match ae with
		| CONST n -> map (fun n' -> CONST n') (QCheck.Shrink.int n)
		| VAR x -> map (fun x' -> VAR x') (QCheck.Shrink.int x)
		| POWER (x, n) -> 
			map (fun x' -> POWER (x', n)) (QCheck.Shrink.int x)
			<+> map (fun n' -> POWER (x, n')) (QCheck.Shrink.int n)
		| TIMES aes -> map (fun aes' -> TIMES aes') (QCheck.Shrink.list ~shrink:shrink_ae aes)
		| SUM aes  -> map (fun aes' -> SUM aes') (QCheck.Shrink.list ~shrink:shrink_ae aes)
	in
  	let rec shrink_env env =
  		QCheck.Shrink.list ~shrink:(QCheck.Shrink.pair (QCheck.Shrink.int) (QCheck.Shrink.int)) env
	in
	pair (pair (shrink_ae ae) (QCheck.Shrink.int x)) (shrink_env env)

let gen : t QCheck.Gen.t 
= 
	let open QCheck.Gen in
	let gen_ae = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> oneof [
				map (fun n -> CONST n) small_int; 
				map (fun x -> VAR n) nat;
				map2 (fun x n -> POWER (x, n)) nat small_int
				]
			| _ -> 
				frequency [
				 1, map (fun n -> CONST n) small_int; 
				 1, map (fun x -> VAR n) nat;
				 1, map2 (fun x n -> POWER (x, n)) nat small_int;
				 1, map (fun aes -> TIMES aes) (list_size (int_range 0 5) (recgen (n/4)));
				 1, map (fun aes -> SUM aes) (list_size (int_range 0 5) (recgen (n/4)));
				])
		)
	in
	let gen_env = list_repeat 5 (pair nat small_int) in
	pair (pair gen_ae nat) gen_env
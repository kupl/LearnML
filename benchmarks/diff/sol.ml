exception Failure of string

type ae =
	| CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec map : (ae * string -> ae) -> ae list * string  -> ae list
= fun f (l, x) ->
	match l with
	| [] -> []
	| hd::tl -> (f (hd, x))::(map f (tl, x))

let rec diff : ae * string -> ae
= fun (e, x) ->
	match e with
	| CONST n -> CONST 0
	| VAR a -> if (a <> x) then CONST 0 else CONST 1
	| POWER (a, n) -> 
		if n < 0 then raise (Failure "Invalid") 
		else if (n = 0) || (a <> x) then CONST 0
		else TIMES [CONST n; POWER (a, n-1)]
	| TIMES l ->
		begin 
			match l with
			| [] -> raise (Failure "Invalid")
			| [hd] -> diff (hd, x)
			| hd::tl -> SUM [TIMES ((diff (hd, x))::tl); TIMES [hd; diff (TIMES tl, x)]]
		end
	| SUM l -> 
		begin match l with
		| [] -> raise (Failure "Invalid")
		| _ -> SUM (map diff (l, x))
		end
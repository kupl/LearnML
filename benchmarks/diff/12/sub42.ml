type ae = CONST of int
            | VAR of string
            | POWER of string * int
            | TIMES of ae list
            | SUM of ae list
			
let rec diff (ae, s) = 
	(* IMPORTANT NOTICE ABOUT COPYRIGHT
			BELOW IS COPIED AND MODIFIED FROM
				"hw3.ml"
					OF
				(*
				   SNU 4190.310 Programming Languages (Fall 2010)
				 
				   K- Interpreter
				*)
	*)
		
	let rec replace_nth = fun l n c -> 
		match l with
		| h::t -> if n = 0 then c::t else h::(replace_nth t (n-1) c)
		| [] -> [] in

	(* COPY AND MODIFICATION END *)
	(* IMPORTANT NOTICE ABOUT COPYRIGHT END *)
	
	match ae with
		| CONST _ -> CONST 0
		| VAR s' ->
			if (s = s')
			then (CONST 1)
			else (CONST 0)
		| POWER (s', i) ->
			if (s = s')
			then (
				if (i = 0)
				then (CONST 0)
				else (TIMES [(CONST i); POWER (s, i - 1)])
			)
			else (CONST 0)
		| TIMES l ->
			let rec func n l l' =
				if (n = List.length l)
				then (l')
				else (func (n + 1) l (l'@
					([TIMES (replace_nth l n (diff (List.nth l n, s)))]))) in
			let l' = func 0 l [] in
			SUM l'
		| SUM l -> SUM (List.map (fun x -> diff (x, s)) l)

(* TEST SET *)
(*
let x = "x";;
let y = "y";;
diff (CONST 1, x);;
diff (VAR x, x);;
diff (POWER (x, -1), x);;
diff (TIMES [(VAR x); (VAR x)], x);;
diff (SUM [(VAR x); (VAR y); TIMES [(VAR x); (VAR x)]], x);;
diff (SUM [(VAR y); (TIMES [(VAR x); (VAR y); (VAR y)])], y);;
*)
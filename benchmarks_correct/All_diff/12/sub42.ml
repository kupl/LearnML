type aexp = Const of int
            | Var of string
            | Power of string * int
            | Times of aexp list
            | Sum of aexp list
			
let rec diff (aexp, s) = 
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
	
	match aexp with
		| Const _ -> Const 0
		| Var s' ->
			if (s = s')
			then (Const 1)
			else (Const 0)
		| Power (s', i) ->
			if (s = s')
			then (
				if (i = 0)
				then (Const 0)
				else (Times [(Const i); Power (s, i - 1)])
			)
			else (Const 0)
		| Times l ->
			let rec func n l l' =
				if (n = List.length l)
				then (l')
				else (func (n + 1) l (l'@
					([Times (replace_nth l n (diff (List.nth l n, s)))]))) in
			let l' = func 0 l [] in
			Sum l'
		| Sum l -> Sum (List.map (fun x -> diff (x, s)) l)

(* TEST SET *)
(*
let x = "x";;
let y = "y";;
diff (Const 1, x);;
diff (Var x, x);;
diff (Power (x, -1), x);;
diff (Times [(Var x); (Var x)], x);;
diff (Sum [(Var x); (Var y); Times [(Var x); (Var x)]], x);;
diff (Sum [(Var y); (Times [(Var x); (Var y); (Var y)])], y);;
*)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

exception Inval

let rec diff : aexp * string -> aexp
= fun (aexp,x) ->
	match aexp with
	| Const n -> Const 0
	| Var k -> if k=x then Const 1 else Const 0
	| Power (k, n) ->
		if k=x then 
			if n=0 then Const 0
			else if n=1 then Const 1
			else Times [Const n ; Power(x, n-1)]
		else Const 0
	| Times l ->
		(
		if (List.mem (Const 0) l) then Const 0
		else 
			match l with
			| [] -> raise Inval
			| hd :: [] -> (diff (hd, x))
			| hd :: tl ->
				match hd with
				| Const 1 -> (diff ((Times tl), x))
				| Const n -> Times [hd; (diff ((Times tl), x))]
				| _ -> Sum ([(Times ((diff (hd, x)):: tl); (Times [hd;(diff ((Times tl), x))]))])
		)
	| Sum l ->
		match l with
		| [] -> raise Inval
		| hd :: [] -> (diff (hd, x))
		| hd :: tl -> Sum [(diff (hd, x)); (diff ((Sum tl),x))]

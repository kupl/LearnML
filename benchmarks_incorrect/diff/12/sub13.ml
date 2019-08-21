
type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list


let rec diff(aexp, x) =
	match aexp with
	| Const n -> Const 0
	| Var v ->
		if v = x then Const 1
		else Const 0
	| Power(v, n) ->
		if v = x then Times [Const n;Power(v, n-1)] 
		else Const 0
(*	| Times l -> 
		let tmp1 = 1 in
		let tmp2 = 0 in
		let rec lstRec1 l =
			match l with
			| [] -> Const 0
			| h::t ->
				match h with
				| Const n ->
					let tmp1 = tmp1 * n
					lstRec1 t
				| Var v ->
					if v = x then
						tmp2 = tmp2 + 1
						lstRec1 t
					else lstRec1 t
				| Power(v, n) ->
					if v = x then 
						tmp2 = tmp2 + n
						lstRec1 t
					else lstRec1 t in
		
		if tmp2 = 0 then Const 0
		else if tmp2 = 1 then tmp1
		else Times [tmp1;Power(v, tmp2-1);tmp2]
*)
	| Sum l ->
		let rec lstRec2 l =
			match l with
			| [] -> Const 0
			| h::t -> Sum[diff(h, x);(lstRec2 t)] in
		lstRec2 l

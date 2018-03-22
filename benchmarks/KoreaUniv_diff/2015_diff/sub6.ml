type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp = 
fun(e, x) ->
	match e with
	| Const v -> Const 0
	| Var y -> if x = y then Const 1 else Const 0
	| Power (p, q) -> 
		if p = x && q = 1 then Const 1
		else if p = x then Times[Const q; Power(p, q-1)]
		else Const 0
	| Sum lst ->
		let rec map : aexp list -> aexp list = fun l ->
			match l with
			| [] -> []
			| hd :: tl -> diff(hd, x) :: map tl
		in Sum (map lst)
	| Times lst ->
		match lst with
		| [] -> Const 0
		| [a] -> diff(a, x)
		| hd :: tl -> Sum [Times (diff(hd, x) :: tl); Times [hd; diff(Times tl, x)]]


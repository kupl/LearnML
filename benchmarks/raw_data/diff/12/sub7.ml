type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec diff : aexp * string -> aexp = fun (expr,str) ->
	match expr with
	Const i -> Const 0
	|Var s -> if str!=s then Const 0
		  else Const 1
	|Power(s,i) ->  if str!=s then Const 0
		 	else Times [Const i; Power(s,i-1)] 
	|Times l -> (match l with
		[] -> Const 0
		|h::t -> (match t with
			[] -> Times [diff(h,str)]
			|_ -> Sum[ Times ((diff(h,str))::t); Times [h;(diff(Times t,str))]]
			)
		)
	|Sum l -> (match l with
		[] -> Const 0
		|h::t -> (match t with
            [] -> diff(h,str)
            |_ -> Sum [diff(h,str);diff(Sum t,str)]
            )
		)



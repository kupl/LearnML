type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec exist: aexp list*string -> int
= fun (l, x) ->
match l with 
[] -> 0
|hd::tl-> match hd with 
	Var v -> if v = x then 1 + (exist (tl, x)) else exist (tl, x)
	|Power (str, i) -> if str = x then 1+(exist (tl, x)) else exist (tl, x)
	|Times lst -> exist (lst, x) + exist (tl, x)
	|Sum lst -> exist (lst, x) + exist (tl, x)
	|_ -> 0 + exist (tl, x) 

let rec modify: aexp list*string -> aexp list
= fun (l, x) ->
match l with 
[] -> []
|hd::tl -> (match hd with
	Const n -> Times[Power(x, 0); Const n]::(modify (tl, x))
	|_ -> hd::(modify (tl, x)))

let normalized: aexp*string -> aexp
= fun (l, x) ->
let modified = modify([l], x) in 
match modified with 
[]->raise (Failure "error")
|hd::tl -> hd

let rec diff: aexp * string -> aexp
= fun (aexp, x) ->
if exist ([aexp], x) > 0 then
(match aexp with
Const n -> Const 0
|Var str -> Const 1
|Power (str, n) -> Times[Const n; Power(str, (n-1))]
|Times l -> (match l with
	[] -> Const 1
	|hd::tl -> (match hd with
		 Const n -> Times[Const n; diff(Times tl, x)]
		|Power(str1, n1) -> if n1=0 then Times[Const 0; diff(Times tl, x)]
								else Times[diff(hd, x); diff(Times tl,x)]
		|_ -> Times[diff(hd, x); diff(Times tl,x)])
	)
|Sum l -> let mod_exp = normalized(aexp, x) in (match mod_exp with 
	Sum l1 -> (match l1 with 
		[] -> Const 0
		|hd::tl -> Sum[diff(hd, x); diff(Sum tl, x)]
	  )
	|_ -> raise (Failure "error")
	)
)else (match aexp with
	Times [] -> Const 1
	|_ -> Const 0)

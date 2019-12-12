type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
match aexp with
| Sum lst ->(match lst with
		| [] -> Const 0
		| hd::tl -> Sum [diff(hd,x); diff(Sum tl,x)])

| Times lst -> (match lst with
		| [] -> Const 0
		| hd::tl -> Sum [Times[diff(hd,x);mul tl]; Times[hd; diff(Times tl, x)]])
| Power (v, n) -> Times[ Const n ; Power (v, n-1)]
| Const n -> Const 0
| Var y -> if Var y = Var x then Const 1 else Var y

and mul: aexp list -> aexp
= fun lst ->
if lst = [] then Const 1
else Times lst

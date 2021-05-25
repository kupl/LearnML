type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
match aexp with
|Const(a) -> Const 0
|Var(y) -> if x=y then Const 1 else Const 0
|Power(y, a) ->
	 if x<>y then Const 0
	 else if a=1 then diff (Var y, x)
	 else Times [Const a;Power(y,(a-1))]
|Times(a::b) -> Times [a;diff (Sum(b),x)]
|Sum(l) ->
	match l with
	|[] -> Const 0
	|h::t -> Sum [(diff (h,x));(diff (Sum(t),x))]

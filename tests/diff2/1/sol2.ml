type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list


let rec diff : aexp * string -> aexp
=fun (aexp,x) -> 
match aexp with
Const a -> Const 0
| Var s -> diff (Power(s,1),x)
| Power (s,a) -> if(s=x) then Times [Const a; Power (s,a-1)] else Const 0
| Times lst -> 
(match lst with
[] -> Const 1 
| h::t -> if(t=[]) then diff(h,x) 
else Sum [Times [h; diff (Times t,x)]; Times (diff (h,x)::t)] ) 

| Sum lst -> 
match lst with 
[] -> Const 0
| h::t -> if(t=[]) then diff (h,x)
else Sum [diff (h,x); diff (Sum t,x)]


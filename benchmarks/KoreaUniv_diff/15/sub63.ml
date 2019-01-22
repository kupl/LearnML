type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
|Const a -> Const 0
|Var x -> Const 1
|Power ("x", a) -> ( match a with
 1 -> Var "x"
 |_ -> Times[Const a; Power ("x", a-1)]
)
|Times l -> ( match l with
 h::t -> Sum[Times[diff(h,"x"); Sum t];Times[h;diff(Sum t,"x")]]
)
|Sum l -> (match l with
 h::t -> Sum[diff(h,"x");diff(Sum t,"x")]
 |a ::_ -> diff(a,"x")
)

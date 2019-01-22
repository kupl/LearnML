type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
    match aexp with
    | Const _ -> Const 0
		| Var variable -> if (variable=x) then Const 1 else Const 0
    | Power (variable,0) -> (Const 0)
		| Power (variable,power) -> if(x=variable) then Times [Const power; Power (variable,power-1)] else Const 0
    | Sum [] -> Const 0
    | Sum [e] -> diff(e,x)
    | Sum (head::tail) -> (Sum [diff(head,x); diff((Sum tail),x)])
    | Times [] -> Const 0
    | Times [e] -> diff(e,x)
    | Times (head::tail) -> Sum [Times (diff(head,x)::tail)
                                ;Times [head; diff((Times tail),x)]];;

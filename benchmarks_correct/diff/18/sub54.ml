type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
        match exp with
        |Const a -> Const 0
        |Var x -> if x = var then Const 1 else Const 0
        |Power (x, y) -> 
            if x = var then Times[Const y;Power (x, y-1)] else Const 0
        |Times l -> begin
          match l with
            |[] -> Const 0
            |h::t -> Sum[Times(diff(h, var)::t); Times[h; diff(Times t, var)]]
          end
        |Sum m ->
           match m with
             |[] -> Const 0
             |h::t -> Sum[diff(h, var); diff((Sum t), var)];;
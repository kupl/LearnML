
   type aexp =
   | Const of int
   | Var of string
   | Power of string * int
   | Times of aexp list
   | Sum of aexp list
 
   let rec diff : aexp * string -> aexp
   = fun (exp, var) ->
   match exp with
   |Const a -> Const a
   |Var x -> Var x (* if x=var then Const 1 else Var x*)
   |Power (x, n) ->
    if x=var then Times[Const n; Power(x, n-1)]
    else Power(x, n)
   |Times l -> begin 
    match l with
    |[] -> raise(Failure "error")
    |h::t -> Sum[Times(diff (h, var)::t) ; diff (h, var)]
    end
   |Sum m ->
    match m with
    |[] -> raise(Failure "error")
    |h::t -> Sum[diff (h, var); diff (h,var)]

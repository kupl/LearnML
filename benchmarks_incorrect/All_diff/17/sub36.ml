type aexp =
    | Const of int
    | Var of string
    | Power of string * int
    | Times of aexp list
    | Sum of aexp list

let rec diff : aexp * string -> aexp 
= fun (exp, var) ->
  match exp with
  | Const i -> Const 0
  | Var x -> if x=var then Const 1 else Const 0
  | Power (_,0) -> Const 0
  | Power (x,1) -> if x=var then Const 1 else Const 0
  | Power (x,i) ->
      if x=var then Times [Const i; Power (var, (i-1))] else Const 0
  | Times (x::xs) ->
    Sum [Times [diff (x, var); Times xs]; Times [x; diff((Times xs), var)]]
  | Times [] -> Const 0
  | Sum (x::xs) ->
    Sum ((diff (x,var))::((function|(Sum x)->x|_->[]) (diff ((Sum xs), var))))
  | Sum [] -> Const 0
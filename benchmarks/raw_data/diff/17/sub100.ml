  (* problem 4*)
  type aexp =
    | Const of int
    | Var of string
    | Power of string * int
     | Times of aexp list
    | Sum of aexp list
  let rec diff : aexp * string -> aexp
    = fun (e,x) ->
    match e with
    |Const a -> Const 0
    |Var y -> if x=y then Const 1 else Var y
    |Power (s, i) -> if s=x 
                     then Times[Const i;Power (s, i-1)]
                     else Power (s, i)
    (*not completed*)   
    |Sum m -> Const 0
    |Times l -> Const 0;;


  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let concat : aexp * aexp -> aexp
  = fun (exp1, exp2) ->
  (match (exp1, exp2) with  
  | (Sum l1, Sum l2) -> Sum (l1 @ l2)
  | (Times l1, Times l2) -> Times (l1 @ l2)
  | _ -> raise (Failure "Invalid operation"))

  let rec count_var : aexp * string -> int
  = fun (exp, var) ->
  (match exp with
  | Const n -> 0
  | Var str -> if str = var then 1 else 0
  | Power (str, n) -> if str = var then 1 else 0
  | Sum lst ->
    (match lst with
    | [] -> 0 
    | hd::tl -> if (count_var (hd, var)) =  1 then 1 else (count_var (Sum tl, var)))
  | Times lst ->
    (match lst with
    | [] -> 0
    | hd::tl -> (count_var (hd, var)) + (count_var (Times tl, var))))  
  
  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
  (match exp with
  | Const n -> Const 0
  | Var str -> if str = var then Const 1 else Const 0
  | Power (str, n) -> 
    if str = var then 
    (match n with
    | n when n = 1 -> Const 1
    | n when n = 0 -> Const 0
    | n when n = 2 -> Times [Const n ; Var str]
    | _ -> Times [Const n ; Power (str, n-1)])
    else Const 0
  | Sum lst -> 
    (match lst with
    | [] -> Sum [Const 0]
    | hd::tl -> if (diff (hd, var)) = Const 0 then (diff ((Sum tl), var)) else concat ((Sum [diff (hd, var)]), diff ((Sum tl), var)))
  | Times lst ->
    (match (count_var (Times lst, var)) with
    | 0 -> Const 0
    | 1 -> (match lst with
          | [] -> raise (Failure "Impossible reach")
          | hd::[] -> Times [diff (hd, var)]
          | hd::tl -> if (count_var (hd, var)) = 1 then concat (Times [diff (hd, var)], Times tl) 
                      else concat (Times [hd], diff (Times tl, var)))
    | _ -> (match lst with
          | [] -> Sum [Const 0]
          | hd::[] -> Sum [diff (hd, var)]
          | hd::tl -> if diff (hd, var) = Const 0 then Sum [Times[hd; diff (Times tl, var)]]
                      else if diff (hd, var) = Const 1 then Sum [Times tl; Times[hd; diff (Times tl, var)]]
                      else Sum [Times[diff (hd, var); Times tl]; Times[hd; diff (Times tl, var)]])))

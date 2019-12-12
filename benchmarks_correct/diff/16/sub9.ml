
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec zero : aexp -> bool
  = fun exp ->
  match exp with
  | Const n -> if n = 0 then true else false
  | Times lst -> 
    (match lst with
    | [] -> true
    | hd::tl ->
      if (zero hd) then true
      else if tl = [] then false
      else (zero (Times tl))
    )
  | Sum lst -> 
    (match lst with
    | [] -> true
    | hd::tl -> if not((zero hd)) then false else (zero (Sum tl))
    )
  | _ -> false

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
  match exp with
  |Const n -> Const 0
  |Var str -> if str = var then Const 1 else Const 0
  |Power (str, n) ->
    (if str = var then
      (if n = 0 then Const 0
      else if n = 1 then Const 1
      else if n = 2 then (Times [Const 2; Var var])
      else (Times [Const n; Power (str, n - 1)])
    )
    else Const 0)
  |Times lst ->
    (match lst with
      | [] -> Const 0
      | hd::tl ->
        if tl = [] then diff (hd, var)
        else
          let product1 = Times [diff (hd, var); Times tl] in
          let zero1 = (zero product1) in
          let product2 = Times [hd; diff (Times tl, var)] in
          let zero2 = (zero product2) in
          if (zero1 && zero2) then Const 0
          else if zero1 then product2
          else if zero2 then product1
          else Sum [product1; product2]
    )
  |Sum lst ->
    (match lst with
      | [] -> Const 0
      | hd::tl ->
        let d1 = diff (hd, var) in
        let zero1 = zero d1 in
        let d2 = diff ((Sum tl), var) in
        let zero2 = zero d2 in
        if (zero1 && zero2) then Const 0
        else if zero1 then d2
        else if zero2 then d1
        else
          (match d2 with
            | Sum lst2 -> Sum (d1::lst2)
            | _ -> Sum [d1; d2]
          )
    )
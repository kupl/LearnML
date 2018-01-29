
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
    match exp with
    | Const n -> Const 0
    | Var str -> if str = var then Const 1 else Const 0
    | Power (str, n) ->
      (if str = var then
        if n = 0 then Const 0
        else Times [Const n; Power (str, n-1)]
      else Const 0)
    | Times lst ->
      (match lst with
      | [] -> Const 0
      | _ -> Sum (product [] lst var))
    | Sum lst ->
      (match lst with
      | [] -> Const 0
      | _ -> Sum (add [] lst var))

  and product : aexp list -> aexp list -> string -> aexp list
  = fun lst1 lst2 var ->
    match lst2 with
    | [] -> []
    | h::t -> Times (lst1 @ [diff (h, var)] @ t)::product (lst1 @ [h]) t var

  and add : aexp list -> aexp list -> string -> aexp list
  = fun lst1 lst2 var ->
    match lst2 with
    | [] -> []
    | h::t -> diff (h, var)::add (lst1 @ [h]) t var
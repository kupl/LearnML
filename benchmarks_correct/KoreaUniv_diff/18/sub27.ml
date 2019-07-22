type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
    | Const a -> Const 0
    | Var a -> 
      if a = x then Const 1
      else Const 0
    | Power (a, p) ->
      if a = x then 
        if p == 1 then Const 1
        else if p == 0 then Const 0
        else Times [Const p; Power (a, (p - 1))]
      else Const 0

    | Times lst ->
      (match lst with
        | hd::tl ->
          if tl == [] then diff (hd, x)
          else Sum (Times (diff (hd, x) :: tl) :: [Times (hd :: [diff ((Times tl), x)] )] )
        | [] -> Const 1)
          
    | Sum lst ->
      (match lst with
        | hd::tl ->
          if tl == [] then diff (hd, x)
          else Sum (diff (hd, x) :: [diff ((Sum tl), x)])
        | [] -> Const 0);;






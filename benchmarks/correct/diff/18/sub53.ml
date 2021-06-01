type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with 
    | Const n -> Const 0
    | Var y -> if y = x then Const 1 else Const 0
    | Power (y, n) -> if y = x then Times [Const n; Power (x, n - 1)] else Const 0
    | Times lst -> 
      begin
      match lst with
        | [y] -> diff (y, x)
        | hd::tl -> Sum [Times (diff (hd, x)::tl); Times [hd; diff (Times tl, x)]]
        | [] -> Const 0
      end
    | Sum lst ->
      let rec diffsum : aexp list -> aexp list
      = fun lst ->
        match lst with 
          | hd::tl -> (diff (hd, x))::(diffsum tl)
          | [] -> [Const 0]
      in Sum (diffsum lst);;
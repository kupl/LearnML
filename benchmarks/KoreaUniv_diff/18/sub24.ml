type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
        match exp with
        |Const c -> Const 0
        |Var v -> if v = x then Const 1 else Var v
        |Power (s, i) -> 
            if s = x then Times [Const i; Power (s, i - 1)] else Power (s, i)
        |Times l -> begin
            match l with
            |hd::tl -> Sum [Times [diff (hd, x); tl]; diff (tl, x)]
            end
        |Sum m ->
          match m with
          |hd::tl -> Sum[diff(hd, x); diff(tl, x)];; 
          
          
          Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1];;
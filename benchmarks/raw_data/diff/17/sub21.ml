(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) ->
    let rec diff_list: aexp * string -> aexp list
    = fun (e, x) -> 
        match e with
        | Sum [Const a] -> [Const 0]
        | Sum (Var x :: tl) -> [Const 1]
        | Sum ((Times [Const a; Var x]) :: tl) -> [Const a]
        | Sum ((Power (x, 1)) :: tl) -> [Const 1]
        | Sum ((Times [Const a; Power (x, 1)]) :: tl) -> [Const a]
        | Sum (Power (x, 2) :: tl) ->
                (Times [Const 2; Var x]) :: (diff_list (Sum tl, x))
        | Sum ((Times [Const a; Power (x, 2)]) :: tl) -> 
                (Times [Const (a*2); Var x]) :: (diff_list (Sum tl, x))
        | Sum (Power (x, n) :: tl) ->
                (Times [Const n; Power (x, (n-1))]) :: (diff_list (Sum tl, x))
        | Sum ((Times [Const a; Power (x, n)]) :: tl) ->
                (Times [Const (a*n); Power (x, (n-1))]) :: (diff_list (Sum tl, x))
        | _ -> raise(Failure "Error")
    in Sum (diff_list(e, x));;
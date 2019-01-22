(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let make_sum e = Sum ([e] @ [Const 0])

let rec f (a, x) = match a with
        | Const n -> Const 0
        | Var y -> if (x = y) then Const 1 else Const 0
        | Power (y, n) -> if (x = y) then (Times [Const n; Power(y, n - 1)]) else Const 0
        | Times l -> (match l with
                | [] -> Const 0
                | hd::tl -> Sum [Times (f (hd, x)::tl) ; Times (hd:: [(f(Times tl, x))])])
        | Sum l -> match l with
                | [] -> Const 0
                | hd::tl -> Sum [f (hd,x); f (Sum tl, x)]


let diff : aexp * string -> aexp
= fun (e,x) -> f(e, x)
